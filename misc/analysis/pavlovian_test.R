pavlovian_test <- tibble(
    block = vector(),
    trial = vector(),
    stimulus_right = vector(),
    stimulus_left = vector(),
    magnitude_right = vector(),
    magnitude_left = vector(),
    original_block_right = vector(),
    original_block_left = vector(),
    same_block = vector(),
    valence_left = vector(),
    valence_right = vector(),
    same_valence = vector(),
    magnitude_pair = vector(),
    session = vector()
)

coins <- c(-1.0, -0.5, -0.01, 0.01, 0.5, 1.0)
coin_cmps <- combn(coins, 2)

# Convert to a dataframe with 15 rows and 2 columns
comparison_df <- tibble(magnitude_right = coin_cmps[1, ], magnitude_left = coin_cmps[2, ])

# Randomly shuffle the rows of the dataframe
set.seed(1234) # Setting seed for reproducibility, if desired

# Randomly swap the left and right options in each row
stop_flag <- 1
while (stop_flag > 0.1) {
    comparison_df <- slice_sample(comparison_df, prop = 1)
    for (i in 1:nrow(comparison_df)) {
        if (runif(1) < 0.5) {
            # 50% chance to swap
            temp <- comparison_df$magnitude_right[i]
            comparison_df$magnitude_right[i] <- comparison_df$magnitude_left[i]
            comparison_df$magnitude_left[i] <- temp
        }
    }
    stop_flag <- abs(mean(
        comparison_df$magnitude_right - comparison_df$magnitude_left
    ))
}

# Display the result
comparison_df %>%
    mutate(
        block = 3,
        trial = row_number(),
        stimulus_right = case_match(
            magnitude_right,
            1 ~ "PIT1.png",
            0.5 ~ "PIT2.png",
            0.01 ~ "PIT3.png",
            -1 ~ "PIT6.png",
            -0.5 ~ "PIT5.png",
            -0.01 ~ "PIT4.png",
        ),
        stimulus_left = case_match(
            magnitude_left,
            1 ~ "PIT1.png",
            0.5 ~ "PIT2.png",
            0.01 ~ "PIT3.png",
            -1 ~ "PIT6.png",
            -0.5 ~ "PIT5.png",
            -0.01 ~ "PIT4.png",
        ),
        original_block_right = NA,
        original_block_left = NA,
        same_block = NA,
        valence_right = sign(magnitude_right),
        valence_left = sign(magnitude_left),
        same_valence = valence_right == valence_left,
        magnitude_pair = NA,
        session = 1
    ) %>%
    select(all_of(colnames(pavlovian_test))) %>%
    jsonlite::toJSON(na = "null", dataframe = "rows")
