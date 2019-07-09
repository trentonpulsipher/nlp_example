library(tidyverse)
library(lubridate)
library(tidytext)

data("stop_words")

# read in conference talks from 1971 - 2018

text <- read_csv("conference_talks.csv")


text %>%
    head(3) %>%
    unnest_tokens(word, talk) %>%
    anti_join(stop_words) %>%
    count(word, sort = T)

