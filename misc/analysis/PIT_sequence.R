library(tidyverse)
library(furrr)
plan(multisession)

PIT_config <- expand_grid(mag = c(5, 2, 1), fr = c(1, 8, 16)) %>%
    mutate(rpp = mag / fr) %>%
    filter(rpp %in% c(0.125, 0.25, 0.625, 1, 2)) %>%
    expand_grid(
        coin = c(1, 0.5, 0.01, -1, -0.5, -0.01)
)

set.seed(12345)
PIT_df <- PIT_config %>%
    mutate(trial = row_number()) %>%
    mutate(trialDuration = runif(n(), 6500, 7500))

PIT_trial_list <-
    future_map(1:10000, function(x) {
        PIT_df %>%
            slice_sample(prop = 1) %>%
            mutate(trial = row_number())
    },
    .options = furrr_options(seed = 12345))

PIT_trial_ac <- PIT_trial_list %>%
    future_map_dbl(
        .f = function(x) {
            arrange(x, trial) %>%
                pull(rpp) %>%
                acf(plot = F) %>%
                pluck("acf") %>%
                {.[2]}
        }, .options = furrr_options(seed = 12345)
    )

old_wrong_acf <- which.min(abs(PIT_trial_ac[PIT_trial_ac > 0]))
PIT_trial_ac[PIT_trial_ac > 0][old_wrong_acf]
smallest_acf <- which.min(abs(PIT_trial_ac))
PIT_trial_ac[smallest_acf]


list(PIT_trial_list[[old_wrong_acf]],
     PIT_trial_list[[smallest_acf]]) %>%
    bind_rows(.id = "batch") %>%
    mutate(trial = row_number()) %>%
    ggplot(aes(x = trial, y = rpp)) +
    geom_line(linewidth = 0.5, color = "gray50") +
    geom_point(aes(size = ordered(mag), color = as.ordered(coin))) +
    scale_size_discrete(range = c(2,5)) +
    scale_color_brewer(type = "div") +
    labs(x = "Trial",
         y = "Reward per press",
         size = "Reward magnitude",
         color = "Coin") +
    theme_minimal()

# PIT_trial_list[PIT_trial_ac > 0][order(abs(PIT_trial_ac[PIT_trial_ac > 0]))[1:2]] %>%
#     bind_rows(.id = "batch") %>%
#     mutate(trial = row_number()) %>%
#     ggplot(aes(x = trial, y = coin)) +
#     geom_line(linewidth = 0.6) +
#     geom_point(aes(size = ordered(mag))) +
#     scale_size_discrete(range = c(1, 4)) +
#     labs(x = "Trial",
#          y = "Coin",
#          size = "Reward magnitude",
#          color = "") +
#     theme_minimal()

list(PIT_trial_list[[old_wrong_acf]],
     PIT_trial_list[[smallest_acf]]) %>%
    bind_rows(.id = "batch") %>%
    mutate(trial = row_number()) %>%
    arrange(trial) %>%
    mutate(trialDuration = round(trialDuration)) %>%
    write_csv("pilot4.x_PIT_sequence.csv")

list(PIT_trial_list[[old_wrong_acf]],
     PIT_trial_list[[smallest_acf]]) %>%
    bind_rows(.id = "batch") %>%
    mutate(trial = row_number()) %>%
    arrange(trial) %>%
    select(magnitude = mag, ratio = fr, coin, trialDuration) %>%
    mutate(trialDuration = round(trialDuration)) %>%
    jsonlite::toJSON(dataframe = "rows")
