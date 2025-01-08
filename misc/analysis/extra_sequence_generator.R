library(furrr)
plan(multisession)

another_rpp_df <- mag_fr_df %>%
    # filter(rpp <= 2.5)
    identity()
set.seed(12345)
another_rpp_df2 <- bind_rows(another_rpp_df, another_rpp_df, another_rpp_df, another_rpp_df, .id = "rep")

another_rpp_by_trial_list <-
    future_map(1:10000, function(x) {
        another_rpp_df2 %>%
            slice_sample(prop = 1, by = rep) %>%
            mutate(trial = row_number())
    },
    .options = furrr_options(seed = 12345))

another_rpp_by_trial_ac <- another_rpp_by_trial_list %>%
    future_map_dbl(
        .f = function(x) {
            arrange(x, trial) %>%
                pull(rpp) %>%
                acf(plot = F) %>%
                pluck("acf") %>%
                {.[2]}
        }, .options = furrr_options(seed = 12345)
    )

which.min(abs(another_rpp_by_trial_ac))
another_rpp_by_trial_ac[which.min(abs(another_rpp_by_trial_ac))]

another_rpp_by_trial_list[[which.min(abs(another_rpp_by_trial_ac))]] %>%
    ggplot(aes(x = trial, y = rpp)) +
    geom_point(aes(size = ordered(mag))) +
    geom_line(linewidth = 0.8) +
    scale_color_brewer(
        type = "qual",
        breaks = c(TRUE, FALSE),
        labels = c("First four trials", "The rest")
    ) +
    scale_size_discrete(range = c(1, 4)) +
    labs(x = "Trial",
         y = "Reward per press",
         size = "Reward magnitude",
         color = "") +
    theme_minimal()

another_rpp_by_trial_list[[which.min(abs(another_rpp_by_trial_ac))]] %>%
    select(magnitude = mag, ratio = fr) %>%
    write_csv("pilot3.2_extra_vigour_sequence_EEG.csv")

