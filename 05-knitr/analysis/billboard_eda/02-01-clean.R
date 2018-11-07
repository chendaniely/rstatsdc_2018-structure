library(readr)
library(dplyr)
library(stringr)

(raw <- readr::read_csv('./data/processed/billboard/billboard.csv') %>%
        dplyr::select(year, artist.inverted, track, time, date.entered,
                      x1st.week:x76th.week) %>%
        dplyr::rename(artist = artist.inverted) %>%
        dplyr::mutate(artist = iconv(artist, "MAC", "ASCII//translit"),
                      track = stringr::str_replace(track,  " \\(.*?\\)", ""),
                      track = dplyr::case_when(
                          nchar(track) > 20 ~ stringr::str_c(stringr::str_sub(track, 0, 20), "..."),
                          TRUE ~ track))  %>%
        dplyr::arrange(year, artist, track)
)

(names(raw)[-(1:5)] <- stringr::str_c("wk", 1:76)) # changed the order here

write_csv(raw, './data/processed/billboard/billboard_clean.csv')
