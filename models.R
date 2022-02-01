# Independent script to fit models

# Setup -------------------------------------------------------------------

library(cmdstanr)
library(brms)
library(tidyverse)

# MCMC settings (ensure at most 12 cores)
ncores <- min(parallel::detectCores(logical = FALSE), 12)
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
fits <- dat %>%
  group_by(outcome) %>%
  nest() %>%
  ungroup()

# Model 1 formulas --------------------------------------------------------


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

# Model 1 fit -------------------------------------------------------------


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
        save_pars = save_pars(group = c("country", "region", "age")),
        file = str_glue("models/brm-{.y}-1-internet")
      )
    )
  )

# Model 2 -----------------------------------------------------------------


# Model formula is only a small update to Model 1
bf_2 <- bf(
  val | resp_se(se, sigma = TRUE) ~
    year * sex * age + i1_cw * sex * age + i1_cb +
    (year * sex * age + i1_cw * sex * age | country) +
    (year * sex * age + i1_cw * sex * age | region),
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

# Model 2 mobile ----------------------------------------------------------


bf_2_m <- bf(
  val | resp_se(se, sigma = TRUE) ~
    year * sex * age + i1_cw * sex * age + m1_cb +
    (year * sex * age + i1_cw * sex * age | country) +
    (year * sex * age + i1_cw * sex * age | region),
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
