library(tidyverse)
library(colorspace)
plot_list <- expand_grid(nesting(
    distance = c(17.5, 25, 35),
    size = c(5, 10, 25),
    chroma = 85 #c(15, 50, 85)
),
nesting(
    hue = c(135, 315),
    shape = c("⬮", "diamond")
),
luminance = 70) %>%
    mutate(color = hex(polarLUV(luminance, chroma, hue)))

plot_background <- function(distance, size, shape, color) {
    grid <- expand_grid(x = seq(0, 125, by = distance), y = x)
    ggplot(grid, aes(x, y)) +
        geom_point(color = color,
                   size = size,
                   shape = shape) +
        coord_fixed(expand = F) +
        theme_void()
}

plots <- dplyr::select(plot_list, distance, size, shape, color) %>%
    arrange(shape, -size) %>%
    pmap(plot_background)
plots %>% wrap_plots(nrow = 2)

iwalk(plots,
      ~ ggsave(
          paste0("PIT", .y, ".png"),
          .x,
          units = "px",
          width = 500,
          height = 500,
          scale = 1,
          dpi = 150
      ))
