library(readr)
library(tidyr)
library(dplyr)
library(stringr)
library(lubridate)

(clean_out <-  read_csv('data/processed/billboard/billboard_clean.csv') %>%
        gather(key = 'week', value = "rank",
                      matches('^wk')) %>%
        mutate(week = as.integer(str_replace_all(week, "[^0-9]+", "")),
                      date.entered = ymd(date.entered),
                      date = date.entered + weeks(week - 1),
                      rank = as.numeric(rank)
        ) %>%
        select(-date.entered) %>%
        arrange(year, artist, track, time, week) %>%
        select(year, artist, time, track, date, week, rank)
)

write_csv(clean_out, './data/processed/billboard/billboard_tidy.csv')
