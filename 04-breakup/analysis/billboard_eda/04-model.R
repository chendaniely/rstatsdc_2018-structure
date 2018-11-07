library(readr)
library(broom)
library(ggplot2)
library(forcats)
library(dplyr)

clean_out <- read_csv('./data/processed/billboard/billboard_tidy.csv')

fit <- lm(rank ~ week, data = clean_out)
(coefs <- tidy(fit))


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
(coefs <- tidy(fit))

ggplot(coefs, aes(x = estimate,
                  y = term)) +
    geom_point() +
    geom_errorbarh(aes(xmin = std.error - (1.96 * estimate),
                       xmax = std.error + (1.96 * estimate))) +
    theme_minimal()
ggsave('./output/billboard_model_plots/coefs_predict_rank_week_artist.png', width = 8, height = 5, units = 'in')

ggplot(coefs[!coefs$term %in% c('(Intercept)'), ],
       aes(x = estimate,
           y = fct_reorder(term, estimate))) +
    geom_point() +
    theme_minimal()
ggsave('./output/billboard_model_plots/coefs_predict_rank_week_artist_sorted.png', width = 8, height = 5, units = 'in')

# Question: Who are the artists that are doing the "best"
(pos <- coefs %>%
        filter(estimate > 0) %>%
        arrange(-estimate, std.error)
)
pos$term

# Question: Who are the artists that are doing the "worst"
(neg <- coefs %>%
        filter(estimate < 0) %>%
        arrange(estimate, std.error)
)
neg$term

# question: are there any "significant" terms?
(coefs %>% filter(p.value <= .05) %>%
        arrange(p.value, estimate)
)

(pos %>% filter(p.value <= .05)
)
