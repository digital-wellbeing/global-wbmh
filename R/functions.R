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

#' Model summary
#'
#' @param x a brmsfit
#'
#' @return A tibble with information on data in the model
msum <- function(x) {
  tibble(
    regions = ngrps(x)$region,
    countries = ngrps(x)$country,
    `year/country` = x$data %>%
      distinct(country, year) %>%
      count(country) %>%
      pull(n) %>%
      median(),
    nobs_internet = standata(x)$N_internet,
    nobs_outcome = standata(x)$N_val,
    outcome_range = str_glue(
      "{number(min(x$data$val), .01)} - {number(max(x$data$val), .01)}"
    )
  )
}

#' Posterior draws at average and grouping levels
#'
#' @param x brmsfit model 1/2
#' @param p name of parameter
#' @param grouping the grouping factor (e.g. region)
#'
#' @return a long tibble of posterior draws of p for every group in grouping (including the average, so useful for e.g. forest plots)
pdraws <- function(x, p = "val_year", grouping = "region") {
  bind_cols(
    tibble("Average" = fixef(x, summary = FALSE)[, p]),
    as_tibble(coef(x, summary = FALSE)[[grouping]][, , p])
  ) %>%
    pivot_longer(everything(), names_to = grouping)
}

#' Country-specific posterior summaries
#'
#' @param x brmsfit model 1/2
#' @param p name of parameter
#'
#' @return a long tibble with posterior summary of p at each level
psums <- function(x, p = "val_year") {
  psum_region <- bind_rows(
    fixef(x) %>%
      as.data.frame() %>%
      rownames_to_column("parameter") %>%
      filter(parameter == {{ p }}) %>%
      select(-parameter) %>%
      mutate(region = "Average"),
    coef(x)$region[, , p] %>%
      as.data.frame() %>%
      rownames_to_column("region")
  ) %>%
    as_tibble()

  psum_country <- ranef(x)$country[, , p] %>%
    as.data.frame() %>%
    rownames_to_column("country") %>%
    left_join(distinct(x$data, region, country)) %>%
    as_tibble() %>%
    left_join(select(psum_region, region, r_value = Estimate)) %>%
    mutate(across(Estimate:Q97.5, ~ . + r_value)) %>%
    select(-r_value)

  bind_rows(
    psum_region,
    psum_country
  ) %>%
    mutate(region = fct_relevel(region, "Average"))
}

#' Get (grand mean) fitted values for countries
#'
#' @param x a brmsfit model 1
#'
#' @return a data frame of fitted values
fitted_country <- function(x) {
  newx <- x$data %>%
    group_by(country, region, year) %>%
    summarise(val = mean(val, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(sex = NA, age = NA, se = 0, itu = TRUE)
  newy <- bind_cols(
    newx,
    as.data.frame(
      fitted(
        x,
        newdata = newx,
        resp = "val",
        re_formula = ~ (year * sex | country) + (year * sex | region)
        )
      )
  )
  newy
}

#' Calculate posterior draws for simple effects (by sex and age) for regions and average
#'
#' @param x brmsfit model 2
#' @param h a brmshypothesis
#'
#' @return a long tibble of draws of posterior answers to hypothesis
simple_effects_samples <- function(x, h) {
  # Calculate hypotheses at average level
  average_h <- hypothesis(
    x, h
  )
  # Give hypotheses appropriate names
  average <- average_h$samples
  names(average) <- interaction(
    "Average", average_h$hypothesis$Hypothesis,
    sep = "+"
  )
  # Pivot to long format, note use of dtplyr
  average <- average %>%
    lazy_dt() %>%
    pivot_longer(everything()) %>%
    separate(name, into = c("region", "Hypothesis"), sep = "\\+") %>%
    as_tibble()

  # Regions as above
  region_h <- hypothesis(
    x, h,
    scope = "coef", group = "region"
  )
  region <- region_h$samples
  names(region) <- interaction(
    region_h$hypothesis$Group,
    region_h$hypothesis$Hypothesis,
    sep = "+"
  )
  region <- region %>%
    lazy_dt() %>%
    pivot_longer(everything()) %>%
    separate(name, into = c("region", "Hypothesis"), sep = "\\+") %>%
    as_tibble()

  bind_rows(average, region)
}

#' Posterior summaries of country- region- and average-level effects for the demographic groups
#'
#' @param x brmsfit model 2
#' @param h character vector of hypotheses
#'
#' @return
simple_effects_summary <- function(x, h) {

  # Calculate hypotheses at average level
  average <- hypothesis(x, h)$hypothesis %>% as_tibble()
  average$region <- "Average"

  # Regions as above
  region <- hypothesis(
    x, h,
    scope = "coef", group = "region"
  )$hypothesis %>%
    as_tibble()
  region <- rename(region, region = Group)

  # Country hypotheses
  country <- hypothesis(
    x, h,
    scope = "ranef", group = "country"
  )
  country <- country$hypothesis %>% as_tibble()
  country <- rename(country, country = Group)

  # Add region coefs
  country <- left_join(country, distinct(x$data, region, country))
  region_coef <- region %>%
    select(region, Hypothesis, r_region = Estimate)
  country <- country %>%
    left_join(region_coef) %>%
    mutate(across(c(Estimate, CI.Lower, CI.Upper), ~ . + r_region)) %>%
    select(-r_region)

  bind_rows(average, region, country)
}
