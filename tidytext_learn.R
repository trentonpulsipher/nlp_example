library(tidyverse)
library(lubridate)
library(tidytext)
library(wordcloud)
library(HSPSUtils)

data("stop_words")

# read in conference talks from 1971 - 2018
text <- read_csv("conference_talks.csv")

# read in sentiments
sentiments_bing <- get_sentiments("bing")


# configure calling
text %>%
    mutate(position = if_else(calling == "President of the Church", "President",
                             if_else(calling %in% c("Of the Quorum of the Twelve Apostles", 
                                                     "First Counselor in the First Presidency",
                                                     "Second Counselor in the First Presidency",
                                                     "Of the Quorum of the Twelve",
                                                     "Acting President of the Quorum of the Twelve Apostles",
                                                     "President of the Quorum of the Twelve Apostles",
                                                     "President of the Quorum of the Tweleve Apostles",
                                                     "Of the Council of the Twelve",
                                                     "Counselor in the First Presidency",
                                                     "Acting President of the Council of the Twelve",
                                                     "Acting President of the Quorum of the Tweleve Apostles",
                                                     "President of the Council of the Twelve",
                                                     "Of the Quorum of the Twelve"), "Apostle", "Other")))

# configure speaker
text %>%
    mutate(
        speaker = str_remove(speaker, "By "),
        speaker = str_remove(speaker, "President "),
        speaker = str_remove(speaker, "Elder "),
        speaker = str_remove(speaker, "Presented by "),
        speaker = gsub("  .*$", "", speaker)
    )




word_sentiment <- text %>%
    # head(10) %>%
    mutate(position = if_else(calling == "President of the Church", "President",
                              if_else(calling %in% c("Of the Quorum of the Twelve Apostles", 
                                                     "First Counselor in the First Presidency",
                                                     "Second Counselor in the First Presidency",
                                                     "Of the Quorum of the Twelve",
                                                     "Acting President of the Quorum of the Twelve Apostles",
                                                     "President of the Quorum of the Twelve Apostles",
                                                     "President of the Quorum of the Tweleve Apostles",
                                                     "Of the Council of the Twelve",
                                                     "Counselor in the First Presidency",
                                                     "Acting President of the Council of the Twelve",
                                                     "Acting President of the Quorum of the Tweleve Apostles",
                                                     "President of the Council of the Twelve",
                                                     "Of the Quorum of the Twelve"), "Apostle", "Other"))) %>%
    mutate(date = dmy(paste("01", conference))) %>%
    unnest_tokens(word, talk) %>%
    anti_join(stop_words) %>%
    inner_join(sentiments_bing) %>%
    group_by(conference, date, speaker, title, url, position) %>%
    count(word, sentiment, sort = T) %>%
    spread(sentiment, n, fill = 0) %>%
    mutate(sentiment = positive - negative) %>%
    summarise(sentiment = sum(sentiment)) %>%
    rownames_to_column() %>%
    mutate(rowname = as.integer(rowname))

word_sentiment %>%
    ggplot(aes(date, sentiment)) +
    # geom_col(show.legend = F) +
    geom_point(alpha = .25, show.legend = F) + 
    geom_smooth() +
# running it by position would require some clean up #
    # facet_wrap(~ position) +
    labs(y = "Sentiment Score", x = "") +
    theme_bw()

word_sentiment %>%
    ungroup() %>%
    mutate(
        speaker = str_remove(speaker, "By "),
        speaker = str_remove(speaker, "President "),
        speaker = str_remove(speaker, "Elder "),
        speaker = str_remove(speaker, "Presented by "),
        speaker = gsub("  .*$", "", speaker)
    ) %>%
    group_by(speaker) %>%
    summarise(avg = mean(sentiment)) %>%
    arrange(desc(avg))

# tf_idf
# The statistic tf-idf is intended to measure how important a word 
    # is to a document in a collection (or corpus) of documents


# compare speakers

talk_words <- text %>% 
    unnest_tokens(word, talk) %>%
    count(speaker, word, sort = T)

total_talk_words <- talk_words %>%
    group_by(speaker) %>%
    summarise(total = sum(n))

talk_words <- talk_words %>%
    left_join(total_talk_words)

talk_words %>%
    ungroup() %>%
    mutate(
        speaker = str_remove(speaker, "By "),
        speaker = str_remove(speaker, "President "),
        speaker = str_remove(speaker, "Elder "),
        speaker = str_remove(speaker, "Presented by "),
        speaker = gsub("  .*$", "", speaker)
    ) %>%
    group_by(speaker) %>%
    sample_n_groups(10) %>%
    ggplot(aes(n/total, fill = speaker)) +
        geom_histogram(show.legend = FALSE) +
        # xlim(NA, 0.0009) +
        facet_wrap(~ speaker, ncol = 2, scales = "free_y")



talk_words %>%
    ungroup() %>%
    mutate(
        speaker = str_remove(speaker, "By "),
        speaker = str_remove(speaker, "President "),
        speaker = str_remove(speaker, "Elder "),
        speaker = str_remove(speaker, "Presented by "),
        speaker = gsub("  .*$", "", speaker)
    ) %>%
    group_by(speaker) %>%
    sample_n_groups(10) %>%
    mutate(
        rank = row_number(), 
        `term frequency` = n/total
    ) %>%
    ggplot(aes(rank, `term frequency`, color = speaker)) + 
        geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) + 
        scale_x_log10() +
        scale_y_log10()


talk_words %>%
    ungroup() %>%
    mutate(
        speaker = str_remove(speaker, "By "),
        speaker = str_remove(speaker, "President "),
        speaker = str_remove(speaker, "Elder "),
        speaker = str_remove(speaker, "Presented by "),
        speaker = gsub("  .*$", "", speaker)
    ) %>%
    group_by(speaker) %>%
    bind_tf_idf(word, speaker, n) %>%
    arrange(desc(tf_idf)) %>%
    mutate(word = factor(word, levels = rev(unique(word)))) %>% 
    group_by(speaker) %>% 
    top_n(15) %>% 
    ungroup() %>%
    ggplot(aes(word, tf_idf, fill = speaker)) +
        geom_col(show.legend = FALSE) +
        labs(x = NULL, y = "tf-idf") +
        facet_wrap(~ speaker, ncol = 2, scales = "free") +
        coord_flip()



