library(readr)
library(dplyr)
library(ggplot2)
library(lubridate)

clean_out <- read_csv('./data/processed/billboard/billboard_tidy.csv')

wk_rnk_avg <- clean_out %>%
    group_by(week) %>%
    summarise(avg_rnk = mean(rank, na.rm = TRUE))

ggplot(wk_rnk_avg, aes(x = week, y = avg_rnk)) +
    geom_line() +
    geom_vline(aes(xintercept = 52), color = 'red') +
    ggtitle(label = 'Average Rank Across All Songs by Week',
                     subtitle = 'Red line shows 52 weeks (1 Year)') +
    xlab('Week') +
    ylab('Average rank') +
    theme_minimal()
ggsave('./output/billboard_rank_plots/avg_rnk_by_week.png', width = 8, height = 5, units = 'in')

clean_out$month <- month(clean_out$date)

mo_rnk_avg <- clean_out %>%
    group_by(month) %>%
    summarise(avg_rnk = mean(rank, na.rm = TRUE))

ggplot(mo_rnk_avg, aes(x = month, y = avg_rnk)) +
    geom_line() +
    scale_x_continuous(breaks = seq(1, 12, 1)) +
    ggtitle('Average Rank Across All Songs by Month') +
    xlab('Month') +
    ylab('Average rank') +
    theme_minimal()
ggsave('./output/billboard_rank_plots/avg_rnk_by_month.png', width = 8, height = 5, units = 'in')

mo_wk_rnk_avg <- clean_out %>%
    group_by(month, week) %>%
    summarise(avg_rnk = mean(rank, na.rm = TRUE)) %>%
    filter(!is.na(avg_rnk)) ## this part is different from original

ggplot(mo_wk_rnk_avg, aes(x = week, y = avg_rnk,
                          color = as.factor(month))) +
    geom_line() +
    ggtitle('Average Rank Across All Songs by Week Across Each Month') +
    xlab('Week') +
    ylab('Average rank') +
    theme_minimal()
ggsave('./output/billboard_rank_plots/avg_rank_by_week_across_months.png',
                width = 8, height = 5, units = 'in')

# question: what is that peak between week 20 and 30?
mo_wk_rnk_avg %>%
    filter(week > 20, week < 30, avg_rnk > 75)
# answer: 27 weeks after a song that was released in december
