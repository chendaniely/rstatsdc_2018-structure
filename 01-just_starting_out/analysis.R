#library(haven)
library(foreign)
#library(dplyr)
library(stringr)
library(plyr)
library(reshape2)


raw <- read.csv("billboard.csv")
raw <- raw[, c("year", "artist.inverted", "track", "time", "date.entered", "x1st.week", "x2nd.week", "x3rd.week", "x4th.week", "x5th.week", "x6th.week", "x7th.week", "x8th.week", "x9th.week", "x10th.week", "x11th.week", "x12th.week", "x13th.week", "x14th.week", "x15th.week", "x16th.week", "x17th.week", "x18th.week", "x19th.week", "x20th.week", "x21st.week", "x22nd.week", "x23rd.week", "x24th.week", "x25th.week", "x26th.week", "x27th.week", "x28th.week", "x29th.week", "x30th.week", "x31st.week", "x32nd.week", "x33rd.week", "x34th.week", "x35th.week", "x36th.week", "x37th.week", "x38th.week", "x39th.week", "x40th.week", "x41st.week", "x42nd.week", "x43rd.week", "x44th.week", "x45th.week", "x46th.week", "x47th.week", "x48th.week", "x49th.week", "x50th.week", "x51st.week", "x52nd.week", "x53rd.week", "x54th.week", "x55th.week", "x56th.week", "x57th.week", "x58th.week", "x59th.week", "x60th.week", "x61st.week", "x62nd.week", "x63rd.week", "x64th.week", "x65th.week", "x66th.week", "x67th.week", "x68th.week", "x69th.week", "x70th.week", "x71st.week", "x72nd.week", "x73rd.week", "x74th.week", "x75th.week", "x76th.week")]
names(raw)[2] <- "artist"

raw$artist <- iconv(raw$artist, "MAC", "ASCII//translit")
raw$track <- str_replace(raw$track, " \\(.*?\\)", "")
names(raw)[-(1:5)] <- str_c("wk", 1:76)
raw <- arrange(raw, year, artist, track)

long_name <- nchar(raw$track) > 20
raw$track[long_name] <- paste0(substr(raw$track[long_name], 0, 20), "...")

head(raw)

library(lubridate)

clean <- melt(raw, id = 1:5, na.rm = T)
clean$week <- as.integer(str_replace_all(clean$variable, "[^0-9]+", ""))
clean$variable <- NULL

clean$date.entered <- lubridate::ymd(clean$date.entered)
clean$date <- clean$date.entered + lubridate::weeks(clean$week - 1)
clean$date.entered <- NULL
clean <- plyr::rename(clean, c("value" = "rank"))
clean <- plyr::arrange(clean, year, artist, track, time, week)
clean <- clean[c("year", "artist", "time", "track", "date", "week", "rank")]

clean_out <- plyr::mutate(clean,
                    date = as.character(date))
head(clean_out)

# normalization

song <- unrowname(unique(clean[c("artist", "track", "time")]))
song$id <- 1:nrow(song)

narrow <- song[1:15, c("id","artist", "track", "time")]
head(narrow)

rank <- plyr::join(clean, song, match = "first")
rank <- rank[c("id", "date", "rank")]
rank$date <- as.character(rank$date)
head(rank)

# eda -----

wk_rnk_avg <- dplyr::group_by(clean_out, week) %>%
    dplyr::summarise(avg_rnk = mean(rank))

ggplot2::ggplot(wk_rnk_avg, aes(x = week, y = avg_rnk)) +
    ggplot2::geom_line() +
    ggplot2::geom_vline(aes(xintercept = 52), color = 'red')

clean_out$month <- lubridate::month(clean_out$date)
head(clean_out)

mo_rnk_avg <- dplyr::group_by(clean_out, month) %>%
    dplyr::summarise(avg_rnk = mean(rank))

ggplot(mo_rnk_avg, aes(x = month, y = avg_rnk)) +
    geom_line()

mo_wk_rnk_avg <- dplyr::group_by(clean_out, month, week) %>%
    dplyr::summarise(avg_rnk = mean(rank))

ggplot(mo_wk_rnk_avg, aes(x = week, y = avg_rnk, color = as.factor(month))) +
    geom_line()


mo_wk_rnk_avg %>%
    dplyr::filter(week > 20, week < 30, avg_rnk > 75)


clean_out

fit <- lm(rank ~ week, data = clean_out)
coefs <- broom::tidy(fit)
coefs

ggplot(coefs, aes(x = estimate, y = term)) +
    geom_point() +
    geom_errorbarh(aes(xmin = std.error - (1.96 * estimate),
                       xmax = std.error + (1.96 * estimate))) +
    theme_minimal()


ggplot(coefs[!coefs$term %in% c('(Intercept)'), ], aes(x = estimate, y = term)) +
    geom_point() +
    geom_errorbarh(aes(xmin = std.error - (1.96 * estimate),
                       xmax = std.error + (1.96 * estimate))) +
    theme_minimal()


fit <- lm(rank ~ week + artist, data = clean_out)
coefs <- broom::tidy(fit)
coefs

library(forcats)


ggplot(coefs, aes(x = estimate, y = term)) +
    geom_point() +
    geom_errorbarh(aes(xmin = std.error - (1.96 * estimate),
                       xmax = std.error + (1.96 * estimate))) +
    theme_minimal()


ggplot(coefs[!coefs$term %in% c('(Intercept)'), ],
       aes(x = estimate, y = forcats::fct_reorder(term, estimate))) +
    geom_point() +
    theme_minimal()


pos <- coefs %>%
    dplyr::filter(estimate > 0) %>%
    dplyr::arrange(-estimate, std.error)
pos
pos$term

pos %>% dplyr::filter(p.value <= .05)
