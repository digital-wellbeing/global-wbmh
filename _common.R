# Set global options here

# Global knitr options
library(ragg)
knitr::opts_chunk$set(
  echo = TRUE,
  warning = FALSE,
  cache = TRUE,
  error = FALSE,
  message = FALSE,
  fig.align = "center",
  fig.width = 8,
  dev = "ragg_png",
  fig.retina = 2
)

# Plotting theme
library(sysfonts)
library(ggplot2)
Font <- "Titillium Web"
if (!(Font %in% font_families())) font_add_google(Font, Font)
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