# Independent script to fit models

# command line argument must be 1-18 to pick a model from the combinations
# of outcomes (6) and models (3)
cmdargs <- commandArgs(trailingOnly = TRUE)
cmdargs <- as.numeric(cmdargs)
cat("Fitting row", cmdargs)

# ---- setup ---------------------------------------------------------------

options(tidyverse.quiet = TRUE)
library(cmdstanr)
library(brms)
library(tidyverse)

# MCMC settings
(ncores <- min(parallel::detectCores(logical = FALSE), 12))
nchains <- 4
options(brms.backend = "cmdstanr")
hmc <- list(
  chains = nchains,
  cores = nchains,
  threads = min(ncores %/% nchains, 2),
  iter = 2500,
  warmup = 1300,
  refresh = 100,
  adapt_delta = .90,
  max_treedepth = 10
)

# Create directory for intermediate files
dir.create("models", FALSE)

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

bf_itu <- bf(
  internet | subset(itu) ~
    year +
    (year |c| country) +
    (year |r| region),
  family = Beta()
)

bf_val <- bf(
  val | se(se, sigma = TRUE) ~
    year * sex +
    (year * sex | c | country) +
    (year * sex | r | region) +
    (year * sex || age) +
    (year * sex || age:country) +
    (year * sex || age:region),
  family = gaussian()
)

bf_1 <- bf_itu + bf_val + set_rescor(FALSE)

# Simplified model for self-harm
bf_val.s <- bf(
  val | se(se, sigma = TRUE) ~
    year * sex +
    (year * sex | c | country) +
    (year * sex | r | region) +
    (year * sex || age) +
    (year * sex || age:region),
  family = gaussian()
)

bf_1.s <- bf_itu + bf_val.s + set_rescor(FALSE)

bf_2 <- bf(
  val | se(se, sigma = TRUE) ~
    (year + i1_cw) * sex +
    ((year + i1_cw) * sex | country) +
    ((year + i1_cw) * sex | region) +
    ((year + i1_cw) * sex || age) +
    ((year + i1_cw) * sex || age:country) +
    ((year + i1_cw) * sex || age:region),
  family = gaussian()
)

bf_2.s <- bf(
  val | se(se, sigma = TRUE) ~
    (year + i1_cw) * sex +
    ((year + i1_cw) * sex | country) +
    ((year + i1_cw) * sex | region) +
    ((year + i1_cw) * sex || age) +
    ((year + i1_cw) * sex || age:region),
  family = gaussian()
)

bf_3 <- bf(
  val | se(se, sigma = TRUE) ~
    (year + m1_cw) * sex +
    ((year + m1_cw) * sex | country) +
    ((year + m1_cw) * sex | region) +
    ((year + m1_cw) * sex || age) +
    ((year + m1_cw) * sex || age:country) +
    ((year + m1_cw) * sex || age:region),
  family = gaussian()
)

bf_3.s <- bf(
  val | se(se, sigma = TRUE) ~
    (year + m1_cw) * sex +
    ((year + m1_cw) * sex | country) +
    ((year + m1_cw) * sex | region) +
    ((year + m1_cw) * sex || age) +
    ((year + m1_cw) * sex || age:region),
  family = gaussian()
)

fits <- fits %>%
  crossing(nesting(bfrm = list(bf_1, bf_2, bf_3), model = 1:3))

fits <- fits %>%
  mutate(
    bfrm = case_when(
      outcome == "Selfharm" & model == 1 ~ list(bf_1.s),
      outcome == "Selfharm" & model == 2 ~ list(bf_2.s),
      outcome == "Selfharm" & model == 3 ~ list(bf_3.s),
      TRUE ~ bfrm
    )
  )



# fit model ---------------------------------------------------------------

hmc$data <- fits$data[[cmdargs]]
hmc$formula <- fits$bfrm[[cmdargs]]
hmc$file <- str_glue("models/brm-{fits$outcome[[cmdargs]]}-{fits$model[[cmdargs]]}")

do.call(brm, hmc)
