# Independent script to fit models

# command line argument must be 1-18 to pick a model from the combinations
# of outcomes (6) and models (4)
cmdargs <- commandArgs(trailingOnly = TRUE)
cmdargs <- as.numeric(cmdargs)
if (length(cmdargs) == 0) cmdargs <- menu(c(1:18, "Abort"), "Choose model")

# ---- setup ---------------------------------------------------------------

library(dplyr)
library(tidyr)
library(brms)

# MCMC settings
options(brms.backend = "rstan")
hmc <- list(
  save_pars = save_pars(group = c("age", "country")),
  chains = 4,
  cores = 4,
  iter = 4000,
  warmup = 2000,
  refresh = 100,
  init = 0,
  control = list(adapt_delta = .95, max_treedepth = 10)
)

# Create directory for intermediate files
dir.create("models", FALSE)

# Save models to $DATA on HPC
path <- Sys.getenv("DATA")
path <- ifelse(
  path == "",
  "models",
  Sys.getenv("DATA")
)

# Load data
dat <- readRDS("data/data-all.rds")

# Set global contrasts
options(contrasts = c(unordered = "contr.sum", ordered = "contr.poly"))

# Nest data frames to fit models programmatically
fits <- dat %>%
  group_by(outcome) %>%
  nest() %>%
  ungroup()


# models ------------------------------------------------------------------

# Time only
bf_t <- bf(
  val | se(se, sigma = TRUE) + subset(!is.na(val)) ~
    year * sex +
    (year * sex | country) +
    (year * sex | age) +
    (year * sex | age:country),
  family = gaussian()
)
# Simplified model for mental health outcomes
bf_t.s <- update(bf_t, ~ . -(year * sex | age:country))

# Univariate outcome on time and internet
bf_i <- bf(
  val | se(se, sigma = TRUE) ~
    (year + i_cw) * sex + i_cb +
    ((year + i_cw) * sex | country) +
    ((year + i_cw) * sex | age) +
    ((year + i_cw) * sex | age:country),
  family = gaussian()
)
bf_i.s <- update(bf_i, ~ . -((year + i_cw) * sex | age:country))

# Univariate outcome on time and mobile
bf_m <- bf(
  val | se(se, sigma = TRUE) ~
    (year + m_cw) * sex + m_cb +
    ((year + m_cw) * sex | country) +
    ((year + m_cw) * sex | age) +
    ((year + m_cw) * sex | age:country),
  family = gaussian()
)
bf_m.s <- update(bf_m, ~ . -((year + m_cw) * sex | age:country))

# Pick the row to fit
fits <- fits %>%
  crossing(nesting(bfrm = list(bf_t, bf_i, bf_m), model = 1:3)) %>%
  slice(cmdargs)

# Simplify models for mental health outcomes
MH <- c("Anxiety", "Depression", "Selfharm")
fits <- fits %>%
  mutate(
    bfrm = case_when(
      outcome %in% MH & model == 1 ~ list(bf_t.s),
      outcome %in% MH & model == 2 ~ list(bf_i.s),
      outcome %in% MH & model == 3 ~ list(bf_m.s),
      TRUE ~ bfrm
    )
  ) %>%
  arrange(outcome, model)

# Priors
p1 <- prior(normal(0, 1), class = "b") +
  prior(normal(0, 3), class = "b", coef = "sex1")
p2 <- prior(normal(0, 1), class = "b") +
  prior(normal(0, 3), class = "b", coef = "sex1") +
  prior(normal(0, 0.5), class = "sd", coef = "i_cw", group = "age") +
  prior(lkj(1), class = "cor", group = "age")
p3 <- prior(normal(0, 1), class = "b") +
  prior(normal(0, 3), class = "b", coef = "sex1") +
  prior(normal(0, 0.5), class = "sd", coef = "m_cw", group = "age") +
  prior(lkj(1), class = "cor", group = "age")


# Center predictors -------------------------------------------------------

# Centering predictors within and between countries
fits$data[[1]] <- fits$data[[1]] %>%
  drop_na(val) %>%
  rename(i = internet, m = mobile) %>%
  group_by(country) %>%
  mutate(
    # Between
    i_cb = mean(i, na.rm = TRUE),
    m_cb = mean(m, na.rm = TRUE),
    # Within
    i_cw = i - i_cb,
    m_cw = m - m_cb
  ) %>%
  ungroup() %>%
  # Grand mean center between-country deviations
  mutate(
    across(c(i_cb, m_cb), ~. - mean(., na.rm = TRUE)),
  )


# fit model ---------------------------------------------------------------

hmc$data <- fits$data[[1]]
hmc$formula <- fits$bfrm[[1]]
hmc$prior <- list(p1, p2, p3)[[fits$model]]
hmc$file <- paste0(path, "/brm-", fits$outcome[[1]], "-", fits$model[[1]])

cat("\n\nNow fitting", hmc$file, "\n\n")

do.call(brm, hmc)
