library(ggplot2)
library(ragg)
library(khroma)

# Plotting options
# If you've defined a custom font, we will use it in outputs
Font <- Sys.getenv("FONT")
theme_set(
  theme_linedraw(
    base_size = 9,
    base_family = Font
  ) +
    theme(
      panel.grid = element_blank(),
      strip.text = element_text(margin = margin(4, 4, 4, 4, "pt")),
      axis.text = element_text(size = rel(0.75))
    )
)
# Width of figures
W <- 9

library(knitr)
opts_chunk$set(
  cache.lazy = FALSE
)

source("R/functions.R")