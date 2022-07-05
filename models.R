# Independent script to fit models

# command line argument must be 1-24 to pick a model from the combinations
# of outcomes (6) and models (4)
cmdargs <- commandArgs(trailingOnly = TRUE)
cmdargs <- as.numeric(cmdargs)
if (length(cmdargs) == 0) cmdargs <- menu(c(1:24, "Abort"), "Choose model")

# ---- setup ---------------------------------------------------------------

options(tidyverse.quiet = TRUE)
library(tidyverse)
library(cmdstanr)
library(brms)

# MCMC settings
(ncores <- min(parallel::detectCores(logical = FALSE), 12))
nchains <- 4
options(brms.backend = "cmdstanr")
hmc <- list(
  chains = nchains,
  cores = nchains,
  threads = min(ncores %/% nchains, 3),
  iter = 2500,
  warmup = 1250,
  refresh = 100,
  adapt_delta = .95,
  max_treedepth = 10
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

# Bivariate internet & outcome on time
bf_itu <- bf(
  internet | subset(itu) ~
    year +
    (year |c| country),
  family = gaussian()
)
bf_val <- bf(
  val | se(se, sigma = TRUE) + subset(!is.na(val)) ~
    year * sex +
    (year * sex | c | country) +
    (year * sex | age) +
    (year * sex | age:country),
  family = gaussian()
)
# Model for well-being outcomes
bf_1 <- bf_itu +
  bf_val +
  set_rescor(FALSE)
# Simplified model for mental health outcomes
bf_1.s <- bf_itu +
  update(bf_val, ~ . -(year * sex | age:country)) +
  set_rescor(FALSE)

# Bivariate mobile & outcome on time
bf_mobile <- bf(
  mobile | subset(itum) ~
    year +
    (year |c| country)
)
bf_2 <- bf_mobile +
  bf_val +
  set_rescor(FALSE)
bf_2.s <- bf_mobile +
  update(bf_val, ~ . -(year * sex | age:country)) +
  set_rescor(FALSE)

# Univariate outcome on time and internet
bf_3 <- bf(
  val | se(se, sigma = TRUE) ~
    (year + i1_cw) * sex + i1_cb +
    ((year + i1_cw) * sex | country) +
    ((year + i1_cw) * sex | age) +
    ((year + i1_cw) * sex | age:country),
  family = gaussian()
)
bf_3.s <- update(bf_3, ~ . -((year + i1_cw) * sex | age:country))

# Univariate outcome on time and mobile
bf_4 <- bf(
  val | se(se, sigma = TRUE) ~
    (year + m1_cw) * sex + m1_cb +
    ((year + m1_cw) * sex | country) +
    ((year + m1_cw) * sex | age) +
    ((year + m1_cw) * sex | age:country),
  family = gaussian()
)
bf_4.s <- update(bf_4, ~ . -((year + m1_cw) * sex | age:country))

# Univariate outcome on time and hdi
bf_5 <- bf(
  val | se(se, sigma = TRUE) ~
    (year + h1_cw) * sex + h1_cb +
    ((year + h1_cw) * sex | country) +
    ((year + h1_cw) * sex | age) +
    ((year + h1_cw) * sex | age:country),
  family = gaussian()
)
bf_5.s <- update(bf_5, ~ . -((year + h1_cw) * sex | age:country))

fits <- fits %>%
  crossing(nesting(bfrm = list(bf_1, bf_2, bf_3, bf_4, bf_5), model = 1:5))

# Simplify models for mental health outcomes
MH <- c("Anxiety", "Depression", "Selfharm")
fits <- fits %>%
  mutate(
    bfrm = case_when(
      outcome %in% MH & model == 1 ~ list(bf_1.s),
      outcome %in% MH & model == 2 ~ list(bf_2.s),
      outcome %in% MH & model == 3 ~ list(bf_3.s),
      outcome %in% MH & model == 4 ~ list(bf_4.s),
      outcome %in% MH & model == 5 ~ list(bf_5.s),
      TRUE ~ bfrm
    )
  ) %>%
  arrange(outcome, model)


# fit model ---------------------------------------------------------------

hmc$data <- fits$data[[cmdargs]]
hmc$formula <- fits$bfrm[[cmdargs]]
hmc$file <- str_glue("models/brm-{fits$outcome[[cmdargs]]}-{fits$model[[cmdargs]]}")

cat("\n\nNow fitting", hmc$file, "\n\n")

do.call(brm, hmc)
