library(readr)
library(dplyr)
library(stringr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(forcats)

# clean -----

(raw <- readr::read_csv('./data/original/billboard.csv') %>%
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

(names(raw)[-(1:5)] <- str_c("wk", 1:76)) # changed the order here

# tidy -----

(clean_out <- raw %>%
    tidyr::gather(key = 'week', value = "rank",
              tidyselect::matches('^wk')) %>%
    dplyr::mutate(week = as.integer(stringr::str_replace_all(week, "[^0-9]+", "")),
                  date.entered = lubridate::ymd(date.entered),
                  date = date.entered + lubridate::weeks(week - 1),
                  rank = as.numeric(rank) ## this is new compared to the original analysis script
                  ) %>%
    dplyr::select(-date.entered) %>%
    dplyr::arrange(year, artist, track, time, week) %>%
    dplyr::select(year, artist, time, track, date, week, rank)
)

readr::write_csv(clean_out, './data/processed/billboard/billboard_clean.csv')

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

readr::write_csv(song, './data/processed/billboard//songs.csv')
readr::write_csv(rank, './data/processed/billboard/rank.csv')

# eda -----

wk_rnk_avg <- clean_out %>%
    dplyr::group_by(week) %>%
    dplyr::summarise(avg_rnk = mean(rank, na.rm = TRUE))

ggplot2::ggplot(wk_rnk_avg, ggplot2::aes(x = week, y = avg_rnk)) +
    ggplot2::geom_line() +
    ggplot2::geom_vline(aes(xintercept = 52), color = 'red') +
    ggplot2::ggtitle(label = 'Average Rank Across All Songs by Week',
                     subtitle = 'Red line shows 52 weeks (1 Year)') +
    ggplot2::xlab('Week') +
    ggplot2::ylab('Average rank') +
    ggplot2::theme_minimal()
ggplot2::ggsave('./output/billboard_rank_plots/avg_rnk_by_week.png', width = 8, height = 5, units = 'in')

clean_out$month <- lubridate::month(clean_out$date)

mo_rnk_avg <- clean_out %>%
    dplyr::group_by(month) %>%
    dplyr::summarise(avg_rnk = mean(rank, na.rm = TRUE))

ggplot(mo_rnk_avg, aes(x = month, y = avg_rnk)) +
    geom_line() +
    scale_x_continuous(breaks = seq(1, 12, 1)) +
    ggtitle('Average Rank Across All Songs by Month') +
    xlab('Month') +
    ylab('Average rank') +
    theme_minimal()
ggplot2::ggsave('./output/billboard_rank_plots/avg_rnk_by_month.png', width = 8, height = 5, units = 'in')

mo_wk_rnk_avg <- clean_out %>%
    dplyr::group_by(month, week) %>%
    dplyr::summarise(avg_rnk = mean(rank, na.rm = TRUE)) %>%
    dplyr::filter(!is.na(avg_rnk)) ## this part is different from original

ggplot(mo_wk_rnk_avg, aes(x = week, y = avg_rnk,
                          color = as.factor(month))) +
    geom_line() +
    ggtitle('Average Rank Across All Songs by Week Across Each Month') +
    xlab('Week') +
    ylab('Average rank')
ggplot2::ggsave('./output/billboard_rank_plots/avg_rank_by_week_across_months.png',
                width = 8, height = 5, units = 'in')

# question: what is that peak between week 20 and 30?
mo_wk_rnk_avg %>%
    dplyr::filter(week > 20, week < 30, avg_rnk > 75)
# answer: 27 weeks after a song that was released in december

# model -----
head(clean_out)


fit <- lm(rank ~ week, data = clean_out)
(coefs <- broom::tidy(fit))


ggplot(coefs, aes(x = estimate, y = term)) +
    geom_point() +
    geom_errorbarh(aes(xmin = std.error - (1.96 * estimate),
                       xmax = std.error + (1.96 * estimate))) +
    theme_minimal()
ggsave('./output/billboard_model_plots/coefs_predict_rank_week.png', width = 8, height = 5, units = 'in')

ggplot(coefs[!coefs$term %in% c('(Intercept)'), ], aes(x = estimate, y = term)) +
    geom_point() +
    geom_errorbarh(aes(xmin = std.error - (1.96 * estimate),
                       xmax = std.error + (1.96 * estimate))) +
    theme_minimal()
ggsave('./output/billboard_model_plots/coefs_predict_rank_week_no_intercept.png', width = 8, height = 5, units = 'in')

fit <- lm(rank ~ week + artist, data = clean_out)
(coefs <- broom::tidy(fit))

ggplot(coefs, aes(x = estimate,
                  y = term)) +
    geom_point() +
    geom_errorbarh(aes(xmin = std.error - (1.96 * estimate),
                       xmax = std.error + (1.96 * estimate))) +
    theme_minimal()
ggsave('./output/billboard_model_plots/coefs_predict_rank_week_artist.png', width = 8, height = 5, units = 'in')

ggplot(coefs[!coefs$term %in% c('(Intercept)'), ],
       aes(x = estimate,
           y = forcats::fct_reorder(term, estimate))) +
    geom_point() +
    theme_minimal()
ggsave('./output/billboard_model_plots/coefs_predict_rank_week_artist_sorted.png', width = 8, height = 5, units = 'in')

# Question: Who are the artists that are doing the "best"
(pos <- coefs %>%
    dplyr::filter(estimate > 0) %>%
    dplyr::arrange(-estimate, std.error)
)
pos$term

# Question: Who are the artists that are doing the "worst"
(neg <- coefs %>%
    dplyr::filter(estimate < 0) %>%
    dplyr::arrange(estimate, std.error)
)
neg$term

# question: are there any "significant" terms?
(coefs %>% dplyr::filter(p.value <= .05) %>%
    dplyr::arrange(p.value, estimate)
)

(pos %>% dplyr::filter(p.value <= .05)
)
