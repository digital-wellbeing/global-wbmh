# Independent script to fit models

# Accept outcome names as command line arguments
cmdargs <- commandArgs(trailingOnly = TRUE)
cat("\n***\nEstimating model for outcome(s)", paste(cmdargs, collapse = ", "), "\n***\n")


# ---- setup ---------------------------------------------------------------

options(tidyverse.quiet = TRUE)
library(cmdstanr)
library(brms)
library(tidyverse)

# MCMC settings (max 12 cores)
(ncores <- min(parallel::detectCores(logical = FALSE), 12))
nchains <- 4
options(brms.backend = "cmdstanr")
hmc <- list(
  chains = nchains,
  cores = nchains,
  threads = ncores %/% nchains,
  iter = 2500,
  warmup = 1250,
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
# Fit models only to outcome(s) specified in command line arguments
fits <- dat %>%
  group_by(outcome) %>%
  nest() %>%
  ungroup()

# Pick Nth row given by command line argument N (1-6)
fits <- fits[cmdargs, ]

# model-1 -----------------------------------------------------------------


bf_itu <- bf(
  internet | subset(itu) ~
    year +
    (year |c| country) +
    (year |r| region),
  family = Beta()
)

bf_1 <- bf(
  val | se(se, sigma = TRUE) ~
    year * sex +
    (year * sex | c | country) +
    (year * sex | r | region) +
    (year * sex || age) +
    (year * sex || age:country) +
    (year * sex || age:region),
  family = gaussian()
)

fits %>%
  mutate(
    fit1 = walk2(
      data, outcome,
      ~brm(
        formula = bf_itu + bf_1 + set_rescor(FALSE),
        data = .x,
        chains = hmc$chains,
        cores = hmc$cores,
        threads = hmc$threads,
        iter = hmc$iter,
        warmup = hmc$warmup,
        refresh = hmc$refresh,
        control = list(
          adapt_delta = hmc$adapt_delta,
          max_treedepth = hmc$max_treedepth
        ),
        file = str_glue("models/brm-{.y}-1-internet")
      )
    )
  )

# model-2 -----------------------------------------------------------------


# Model formula is only a small update to Model 1
bf_2 <- bf(
  val | se(se, sigma = TRUE) ~
    (year + i1_cw) * sex + i1_cb +
    ((year + i1_cw) * sex | country) +
    ((year + i1_cw) * sex | region) +
    ((year + i1_cw) * sex || age) +
    ((year + i1_cw) * sex || age:country) +
    ((year + i1_cw) * sex || age:region),
  family = gaussian()
)

# As above with model 1
# Note we don't need to save these in the R environment, just files
fits %>%
  mutate(
    fit2 = walk2(
      data, outcome,
      ~brm(
        formula = bf_2,
        data = .x,
        chains = hmc$chains,
        cores = hmc$cores,
        threads = hmc$threads,
        iter = hmc$iter,
        warmup = hmc$warmup,
        refresh = hmc$refresh,
        control = list(
          adapt_delta = hmc$adapt_delta,
          max_treedepth = hmc$max_treedepth
        ),
        file = str_glue("models/brm-{.y}-2-internet")
      )
    )
  )

# model-2-m ---------------------------------------------------------------


bf_2_m <- bf(
  val | se(se, sigma = TRUE) ~
    (year + m1_cw) * sex + m1_cb +
    ((year + m1_cw) * sex | country) +
    ((year + m1_cw) * sex | region) +
    ((year + m1_cw) * sex || age) +
    ((year + m1_cw) * sex || age:country) +
    ((year + m1_cw) * sex || age:region),
  family = gaussian(),
  unused = ~outcome
)

fits %>%
  mutate(
    fit2_m = walk2(
      data, outcome,
      ~brm(
        formula = bf_2_m,
        data = .x,
        chains = hmc$chains,
        cores = hmc$cores,
        threads = hmc$threads,
        iter = hmc$iter,
        warmup = hmc$warmup,
        refresh = hmc$refresh,
        control = list(
          adapt_delta = hmc$adapt_delta,
          max_treedepth = hmc$max_treedepth
        ),
        file = str_glue("models/brm-{.y}-2-mobile")
      )
    )
  )
