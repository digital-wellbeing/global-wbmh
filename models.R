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
bf_1 <- bf_itu + bf_val + set_rescor(FALSE)

# Bivariate mobile & outcome on time
bf_mobile <- bf(
  mobile | subset(itum) ~
    year +
    (year |c| country)
)
bf_2 <- bf_mobile + bf_val + set_rescor(FALSE)

# Univariate outcome on time and internet
bf_3 <- bf(
  val | se(se, sigma = TRUE) ~
    (year + i1_cw) * sex +
    ((year + i1_cw) * sex | country) +
    ((year + i1_cw) * sex | age) +
    ((year + i1_cw) * sex | age:country),
  family = gaussian()
)

# Univariate outcome on time and mobile
bf_4 <- bf(
  val | se(se, sigma = TRUE) ~
    (year + m1_cw) * sex +
    ((year + m1_cw) * sex | country) +
    ((year + m1_cw) * sex | age) +
    ((year + m1_cw) * sex | age:country),
  family = gaussian()
)


fits <- fits %>%
  crossing(nesting(bfrm = list(bf_1, bf_2, bf_3, bf_4), model = 1:4))

# fit model ---------------------------------------------------------------

hmc$data <- fits$data[[cmdargs]]
hmc$formula <- fits$bfrm[[cmdargs]]
hmc$file <- str_glue("models/brm-{fits$outcome[[cmdargs]]}-{fits$model[[cmdargs]]}")
hmc$prior <- NULL

# Assign prior for mental health outcomes
if (fits$outcome[[cmdargs]] %in% c("Anxiety", "Depression", "Selfharm"))
  hmc$prior <- prior(
    lkj(6), class = cor, group = age
  ) +
  prior(
    lkj(6), class = cor, group = age:country
  ) +
  prior(
    student_t(7, 0, .2), class = sd, group = age:country, resp = val,
    coef = "Intercept"
  ) +
  prior(
    student_t(7, 0, .02), class = sd, group = age:country, resp = val,
    coef = "sex1"
  ) +
  prior(
    student_t(7, 0, .05), class = sd, group = age:country, resp = val,
    coef = "year"
  ) +
  prior(
    student_t(7, 0, .02), class = sd, group = age:country, resp = val,
    coef = "year:sex1"
  )

cat("Now fitting", hmc$file)

do.call(brm, hmc)
