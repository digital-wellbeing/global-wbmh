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
options(brms.backend = "rstan") # rstan to save subsets of parameters
hmc <- list(
  save_pars = save_pars(group = c("age", "country")),
  chains = 4,
  cores = 4,
  iter = 5000,
  warmup = 2500,
  refresh = 100,
  # init = 0,
  control = list(adapt_delta = .99, max_treedepth = 10)
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
  val | se(se, sigma = TRUE) ~
    year * sex +
    (year * sex | country) +
    (year * sex | age) +
    (year * sex | age:country),
  family = gaussian()
)
# Simplified model for mental health outcomes
bf_t.s <- bf(
  val | se(se, sigma = FALSE) ~
    year * sex +
    (year * sex | country) +
    (year * sex | age),
  family = gaussian()
)

# Time and internet
bf_i <- bf(
  val | se(se, sigma = TRUE) ~
    (i_cw + year) * sex + i_cb +
    ((i_cw + year) * sex | country) +
    ((i_cw + year) * sex | age) +
    ((i_cw + year) * sex | age:country),
  family = gaussian()
)
# Simplified model for mental health outcomes
bf_i.s <- bf(
  val | se(se, sigma = FALSE) ~
    (i_cw + year) * sex + i_cb +
    ((i_cw + year) * sex | country) +
    ((i_cw + year) * sex | age),
  family = gaussian()
)

# Time and mobile
bf_m <- bf(
  val | se(se, sigma = TRUE) ~
    (m_cw + year) * sex + m_cb +
    ((m_cw + year) * sex | country) +
    ((m_cw + year) * sex | age) +
    ((m_cw + year) * sex | age:country),
  family = gaussian()
)
# Simplified model for mental health outcomes
bf_m.s <- bf(
  val | se(se, sigma = FALSE) ~
    (m_cw + year) * sex + m_cb +
    ((m_cw + year) * sex | country) +
    ((m_cw + year) * sex | age),
  family = gaussian()
)

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
# p1 <- prior(normal(0, 1), class = "b") +
#   prior(normal(0, 3), class = "b", coef = "sex1") +
#   prior(normal(0, 0.5), class = "sd", group = "age") +
#   prior(normal(0, 0.5), class = "sd", group = "country")
#
# p2 <- prior(normal(0, 1), class = "b") +
#   prior(normal(0, 3), class = "b", coef = "sex1") +
#   prior(normal(0, 0.5), class = "sd", group = "age") +
#   prior(normal(0, 0.5), class = "sd", group = "country")
#
# p3 <- prior(normal(0, 1), class = "b") +
#   prior(normal(0, 3), class = "b", coef = "sex1") +
#   prior(normal(0, 0.5), class = "sd", group = "age") +
#   prior(normal(0, 0.5), class = "sd", group = "country")

# Center predictors -------------------------------------------------------

# Centering predictors within and between countries
fits$data[[1]] <- fits$data[[1]] %>%
  drop_na(val) %>%
  rename(i = internet, m = mobile) %>%
  mutate(
    # Between
    i_cb = mean(i, na.rm = TRUE),
    m_cb = mean(m, na.rm = TRUE),
    # Within
    i_cw = i - i_cb,
    m_cw = m - m_cb,
    .by = country
  ) %>%
  # Grand mean center between-country deviations
  mutate(
    across(c(i_cb, m_cb), ~. - mean(., na.rm = TRUE)),
  )

# fit model ---------------------------------------------------------------

hmc$data <- fits$data[[1]]
hmc$formula <- fits$bfrm[[1]]
# hmc$prior <- list(p1, p2, p3)[[fits$model]]
hmc$file <- paste0(path, "/brm-", fits$outcome[[1]], "-", fits$model[[1]])

cat("\n\nNow fitting", hmc$file, "\n\n")

do.call(brm, hmc)
