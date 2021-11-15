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
library(ggplot2)
# Use better font if available
Font <- "Titillium Web"
if (require(extrafont) & !(Font %in% extrafont::fonts())) Font <- ""
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