library(tidyverse)
library(colorspace)
library(patchwork)
generate_pavlovian_stim <- function(shapes, hues = NULL, luminance = 70, chroma = 85, wk = NULL) {
    plot_list <- expand_grid(nesting(
        distance = c(17.5, 25, 35),
        size = c(5, 10, 25),
        chroma = chroma #c(15, 50, 85)
    ),
    nesting(
        hue = hues,
        shape = shapes
    ),
    luminance = luminance) %>%
        mutate(color = hex(polarLUV(luminance, chroma, hue), fixup = T))

    plot_background <- function(distance, size, shape, color) {
        grid <- expand_grid(x = seq(0, 125, by = distance), y = x)
        ggplot(grid, aes(x, y)) +
            geom_point(color = color,
                       fill = color,
                       size = size,
                       shape = shape,
                       stroke = 2) +
            coord_fixed(expand = F) +
            theme_void()
    }

    plots <- select(plot_list, distance, size, shape, color) %>%
        arrange(shape, -size) %>%
        pmap(plot_background)
    plots[(length(plots)/2 + 1):length(plots)] <- rev(plots[(length(plots)/2 + 1):length(plots)])
    plots %>% wrap_plots(nrow = 2)

    path <- ifelse(is.null(wk), "PIT", paste0(wk, "/PIT"))
    iwalk(plots,
          ~ ggsave(
              paste0(path, .y, ".png"),
              .x,
              units = "px",
              width = 500,
              height = 500,
              scale = 1,
              dpi = 150
          ))
}

# Wk 0
generate_pavlovian_stim(c("circle", "triangle"), hues = c(58, 237), wk = "wk0")

# Wk 2
generate_pavlovian_stim(c("⬮", "diamond"), hues = c(135, 315), wk = "wk2")

# Wk 4
generate_pavlovian_stim(c("◩", "◒"), hues = c(200, 20), wk = "wk4")

# Wk 24
generate_pavlovian_stim(c(25, 21), c(300, 120), wk = "wk24")

# Wk 28
generate_pavlovian_stim(c("▰", "⬬"), c(40, 220), wk = "wk28")

# EEG s1
generate_pavlovian_stim(c(13, 12), c(180, 0), wk = "EEG1")

# EEG s2
generate_pavlovian_stim(c(14, 10), c(270, 90), wk = "EEG2")

# Colorspace plot
hclplot(hex(fixup = T, polarLUV(H = c(58, 237, 135, 315, 200, 20, 300, 120, 40, 220, 180, 0, 270, 90), C = 85, L = 70)))
specplot(hex(fixup = T, polarLUV(H = c(58, 237, 135, 315, 200, 20, 300, 120, 40, 220, 180, 0, 270, 90), C = 85, L = 70)))
