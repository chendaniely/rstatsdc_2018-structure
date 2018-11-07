library(readr)
library(dplyr)

clean_out <- read_csv('./data/processed/billboard/billboard_tidy.csv')

(song <- clean_out %>%
    select(artist, track, time) %>%
    distinct() %>%
    mutate(id = 1:n())
)

(rank <- clean_out %>%
        left_join(song, by = c( 'artist', 'time', 'track')) %>%
        select(id, date, week, rank)
)

write_csv(song, './data/processed/billboard//songs.csv')
write_csv(rank, './data/processed/billboard/rank.csv')
