library(tidyverse)
library(lubridate)
library(tidytext)
library(wordcloud)


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

ggplot(word_sentiment, aes(date, sentiment)) +
#    geom_col(show.legend = F) +
    geom_point(alpha = .25, show.legend = F) + 
    geom_smooth() +
    facet_wrap(~ position) +
    labs(y = "Sentiment Score", x = "") +
    theme_bw()
