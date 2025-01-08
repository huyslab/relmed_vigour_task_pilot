library(tidyverse)
library(furrr)
plan(multisession)

PIT_config <- expand_grid(mag = c(5, 2, 1), fr = c(1, 8, 16)) %>%
    mutate(rpp = mag / fr) %>%
    filter(rpp %in% c(0.125, 0.25, 0.625, 1), !(mag == 2 & fr == 16)) %>%
    expand_grid(
        coin = c(1, 0.5, 0.01, -1, -0.5, -0.01)
)

set.seed(12345)
PIT_df <- PIT_config %>%
    mutate(trialDuration = runif(n(), 6500, 7500))

PIT_trial_list <-
    future_map(1:100000, function(x) {
        PIT_df %>%
            slice_sample(prop = 1, by = c(mag, fr, rpp)) %>%
            mutate(group = rep(seq(1, length.out = n()/2), each = 2)) %>%
            nest(.by = group) %>%
            slice_sample(prop = 1) %>%
            unnest(data) %>%
            mutate(trial = row_number())
    },
    .options = furrr_options(seed = 12345))

PIT_trial_ac <- PIT_trial_list %>%
    future_map_dbl(
        .f = function(x) {
            arrange(x, trial) %>%
                select(rpp, coin) %>%
                acf(plot = F) %>%
                pluck("acf") %>%
                {abs(.[2,1,1]) + abs(.[2,2,2]) + abs(.[2,1,2]) + abs(.[2,2,1])}
        }, .options = furrr_options(seed = 12345)
    )

# which.min(abs(PIT_trial_ac))
# PIT_trial_ac[which.min(abs(PIT_trial_ac))]

seq_indx <- c(which(rank(abs(PIT_trial_ac)) == 1), which(rank(abs(PIT_trial_ac)) == 2))
PIT_trial_ac[seq_indx]

set.seed(12345)
padded_PIT_sequence <- bind_rows(PIT_trial_list[seq_indx], .id = "rep") %>%
    mutate(group = (trial - 1) %/% 2 + 1) %>%
    group_by(rep, group, mag, fr, rpp) %>%
    group_modify( ~ bind_rows(tibble(
        coin = 0,
        trialDuration = runif(1, 3750, 4250),
        trial = 0
    ), .x)) %>%
    ungroup() %>%
    mutate(trial = row_number()) %>%
    mutate(across(c(mag, fr), ~ if_else(near(rpp, 1/8), .x * 2, .x)))

padded_PIT_sequence %>%
    ggplot(aes(x = trial, y = rpp)) +
    geom_line(linewidth = 0.5, color = "gray50") +
    geom_point(aes(size = ordered(mag),
                   color = as.ordered(coin),
                   shape = as.ordered(fr))) +
    scale_size_discrete(range = c(2,5)) +
    scale_color_brewer(type = "div") +
    labs(x = "Trial",
         y = "Reward per press",
         size = "Reward magnitude",
         shape = "Fixed ratio",
         color = "Coin")

padded_PIT_sequence %>%
    arrange(trial) %>%
    mutate(trialDuration = round(trialDuration)) %>%
    write_csv("pilot7_PIT_sequence.csv")

padded_PIT_sequence %>%
    arrange(trial) %>%
    select(magnitude = mag, ratio = fr, coin, trialDuration) %>%
    mutate(trialDuration = round(trialDuration)) %>%
    jsonlite::toJSON(dataframe = "rows")
