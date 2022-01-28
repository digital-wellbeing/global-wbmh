library(ggplot2)
# Plotting theme & use this font if available
Font <- "Titillium Web"
theme_set(
  theme_linedraw(
    base_size = 12,
    base_family = Font
  ) +
    theme(
      panel.grid = element_blank(),
      strip.text = element_text(margin = margin(4, 4, 4, 4, "pt"))
    )
)

library(knitr)
opts_chunk$set(
  cache.lazy = FALSE
)

source("R/functions.R")