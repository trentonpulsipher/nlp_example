
library(tidyverse)
library(lubridate)
library(tidytext)
library(wordcloud)
data("stop_words")


# read in conference talks from 1971 - 2018
text <- read_csv("conference_talks.csv")

# add date field
text %>% 
    mutate(date = dmy(paste("01", conference))) %>%
    group_by(date) %>%
    summarise(number_talks = n()) %>%
    ggplot(aes(x = date, y = number_talks)) +
        geom_point() +
        theme_bw() +
        labs(x = "", y = "Number of Talks per Conference") +
        geom_smooth()

# count number of words
text %>% 
    mutate(date = dmy(paste("01", conference))) %>%
    group_by(date) %>%
    summarise(number_words = ) %>%
    ggplot(aes(x = date, y = number_talks)) +
    geom_point() +
    theme_bw() +
    labs(x = "", y = "Number of Talks per Conference") +
    geom_smooth()

# wordcloud titles
text %>%
    unnest_tokens(word, title) %>%
    anti_join(stop_words) %>%
    count(word, sort = T) %>%
    with(wordcloud(word, n, max.words = 100))
    
# wordcloud speakers
