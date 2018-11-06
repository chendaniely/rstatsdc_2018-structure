library(readr)
library(dplyr)
library(stringr)
library(tidyr)
library(lubridate)
library(ggplot2)

# set working directory -----
# do not need this anymore becuase of project
# setwd('~/git/hub/rstatsdc_2018-structure/01-just_starting_out/') # linux

# clean -----

(raw <- readr::read_csv('billboard.csv') %>%
    dplyr::select(year, artist.inverted, track, time, date.entered,
                  x1st.week:x76th.week) %>%
    dplyr::rename(artist = artist.inverted) %>%
    dplyr::mutate(artist = iconv(artist, "MAC", "ASCII//translit")) %>%
    dplyr::mutate(track = stringr::str_replace(track,  " \\(.*?\\)", "")) %>%
    dplyr::arrange(year, artist, track) %>%
    dplyr::mutate(track = dplyr::case_when(
        nchar(track) > 20 ~ stringr::str_c(stringr::str_sub(track, 0, 20), "..."),
        TRUE ~ track
    ))
)

# anyone know how to add this to the pipeline?
(names(raw)[-(1:5)] <- str_c("wk", 1:76)) # changed the order here

# tidy -----

(clean_out <- raw %>%
    tidyr::gather(key = 'week', value = "value", # can rename rank = value here
              tidyselect::matches('^wk')) %>%
    dplyr::mutate(week = as.integer(stringr::str_replace_all(week, "[^0-9]+", "")),
                  date.entered = lubridate::ymd(date.entered),
                  date = date.entered + lubridate::weeks(week - 1)
                  ) %>%
    dplyr::select(-date.entered) %>%
    dplyr::rename('rank' = 'value') %>%
    dplyr::arrange(year, artist, track, time, week) %>%
    dplyr::select(year, artist, time, track, date, week, rank)
)

# normalize----

(song <- clean_out %>%
    dplyr::select(artist, track, time) %>%
    dplyr::distinct() %>%
    dplyr::mutate(id = 1:dplyr::n())
)

(rank <- clean_out %>%
    dplyr::left_join(song, by = c( 'artist', 'time', 'track')) %>%
    dplyr::select(id, date, week, rank)
)

