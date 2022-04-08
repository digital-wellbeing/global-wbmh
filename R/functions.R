#' Custom kableExtra formatting
#'
#' @param a table from kbl()
#'
#' @return a table
kable_custom <- function(x) {
  kable_classic(
    kable_input = x,
    html_font = Font,
    font_size = 13,
    lightable_options = "striped",
    position = "center",
    full_width = FALSE
  )
}
