# ----------------------------------------------------
#  Pooling the results from 
#  Reeves, Rogowski, Seo, and Stone:
#  "The Contextual Determinants of Support for Unilateral Action"
#  Fig. 3 (varying presidential power instrument)
# ----------------------------------------------------

# workhorse
library("here")
library("magrittr")
library("tidyverse")
library("scales")
library("broom")

# commarobust()
# library("estimatr")

# bayes
library("rstan")
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)
library("tidybayes")
# library("ggmcmc")
# library("brms")

library("boxr")
box_auth()



# ---- raw data -----------------------

raw <- 
  tribble(
    ~ treatment, ~ estimate, ~ conf.low, ~ conf.high,
    "Exec.\nAgreements", 0.090, 0.033, 0.146,
    "Exec.\nOrders", 0.064, 0.007, 0.120,
    "Nat. Sec.\nDirectives", 0.057, -0.0001, 0.113,
    "Cabinet", 0.057, 0.003, 0.110,
    "Military", 0.053, -0.001, 0.107,
    "Proclamations", 0.032, -0.018, 0.082,
    "Memoranda", 0.024, -0.030, 0.078
  ) %>%
  mutate(treatment = factor(treatment, levels = unique(treatment))) %>%
  group_by() %>%
  mutate(MOE = c(conf.high - estimate, conf.low - estimate) %>%
                    abs() %>%
                    mean(),
         std_error = MOE / 1.645,
         conf.low = estimate - (1.96 * std_error),
         conf.high = estimate + (1.96 * std_error)) %>%
  print()





# ---- sampler hyperparameters -----------------------
# leave one core open
(n_chains <- min(c(parallel::detectCores() - 1, 10)))
n_iterations <- 3000
# n_warmup <- 500
n_thin <- 1

# black box all the sampling params
estimate <- function(model, data) {
  sampling(object = model, 
           data = data, 
           iter = n_iterations, 
           thin = n_thin, 
           chains = n_chains,
           control = list(adapt_delta = 0.9),
           # pars = c("theta", "cutpoint", "discrimination", "dispersion",
           #          "sigma_in_g"
                    # "theta_hypermean", "scale_theta", "z_theta", 
                    # "sigma_g_hypermean", "sigma_in_g", "scale_sigma", "z_sigma", 
                    # "party_int", "party_int_sigma",
                    # "party_coefs", "party_coefs_sigma"
                    # ),
           verbose = TRUE)
}




# creating data
stan_data <- raw %>%
  select(-contains("conf"), -MOE, -treatment) %>%
  rename(y = estimate, sigma = std_error) %>%
  compose_data(n_j = n) %>%
  print()


# ---- models -----------------------
# two dimensions: 
# 1. Normal treatments (0, sd) vs t treatments (3, 0, sd)
# 2. SD family, cauchy(0, 1) vs uniform(0, 10)

pool_normal_cauchy <- here("code", "reeves", "schools-normal-cauchy.stan") %>%
  stan_model(verbose = TRUE)

pool_normal_uniform <- here("code", "reeves", "schools-normal-uniform.stan") %>%
  stan_model(verbose = TRUE)

pool_t_cauchy <- here("code", "reeves", "schools-t-cauchy.stan") %>%
  stan_model(verbose = TRUE)

pool_t_uniform <- here("code", "reeves", "schools-t-uniform.stan") %>%
  stan_model(verbose = TRUE)

beepr::beep(2)

pool_normal_cauchy
pool_normal_uniform
pool_t_cauchy
pool_t_uniform

# out-dated
# eight_schools <- 
#   stan_model(here("code", "replication", "reeves-8-schools.stan"), 
#              verbose = TRUE)

# pool_reeves <- estimate(eight_schools, stan_data)


# ---- estimate all models -----------------------

mcmc_normal_cauchy <- estimate(model = pool_normal_cauchy, data = stan_data)
mcmc_normal_uniform <- estimate(model = pool_normal_uniform, data = stan_data)
mcmc_t_cauchy <- estimate(model = pool_t_cauchy, data = stan_data)
mcmc_t_uniform <- estimate(model = pool_t_uniform, data = stan_data)



all_tidy <- 
  bind_rows(
    "normal_cauchy" = tidy(mcmc_normal_cauchy, conf.int = TRUE),
    "normal_uniform" = tidy(mcmc_normal_uniform, conf.int = TRUE),
    "t_cauchy" = tidy(mcmc_t_cauchy, conf.int = TRUE),
    "t_uniform" = tidy(mcmc_t_uniform, conf.int = TRUE),
    .id = "specification"
  ) %>%
  mutate(
    j = parse_number(term)
  ) %>%
  print()


treats <- all_tidy %>%
  mutate(
    treatment = case_when(is.na(j) ~ "Overall Effect", 
                         TRUE ~ levels(raw$treatment)[j]) %>%
                factor(levels = c(levels(raw$treatment), "Overall Effect"))
  ) %>%
  filter(
    term != "tau",
    str_detect(term, "tilde") == FALSE) %>%
  print()

saveRDS(treats, here("data", "processed", "reeves-treatments.Rds"))



ggplot(treats, aes(x = (treatment), y = estimate)) +
  geom_hline(yintercept = 0) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high, 
                      color = specification),
                  position = position_dodge(width = 0.5),
                  # shape = 21
                  ) +
  # scale_fill_manual(values = c("black", "white")) +
  viridis::scale_color_viridis(option = "magma", discrete = TRUE,
                               end = 0.85) +
  theme_minimal()







combined <- tidy(pool_reeves, conf.int = TRUE) %>%
  filter(term != "tau",
         str_detect(term, "tilde") == FALSE) %>%
  mutate(
    j = parse_number(term), 
    treatment = case_when(is.na(j) ~ "Overall Effect", 
                          TRUE ~ levels(raw$treatment)[j])
  ) %>%
  bind_rows("Bayes" = ., "OLS" = raw, .id = "Model") %>%
  mutate(
    treatment = 
      factor(treatment, c(levels(raw$treatment), "Overall Effect"))
  ) %>%
  print()



ggplot(combined, aes(x = fct_rev(treatment), y = estimate)) +
  geom_hline(yintercept = 0) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high, 
                      color = Model),
                  position = position_dodge(width = -0.25),
                  # shape = 21
                  ) +
  # scale_fill_manual(values = c("black", "white")) +
  viridis::scale_color_viridis(option = "magma", discrete = TRUE,
                               end = 0.65) +
  coord_flip() +
  theme_minimal()

saveRDS(combined, here("data", "processed", "reeves-et-al.Rds"))


# ----------------------------------------------------
#   prior predictive checks?
# ----------------------------------------------------

# simulate style
ppc_reeves <- 
  tibble(
    mu = runif(1000000, -1, 1)
  ) %>%
  mutate(
    beta_hat = rnorm(n(), 0, 0.033),
    cauchy_sd = abs(rcauchy(n(), 0, 1)),
    beta_norm_unif = rnorm(n(), mean = 0, sd = 1) * runif(n(), 0, 1),
    beta_norm_cauchy = rnorm(n(), mean = 0, sd = 1) * cauchy_sd ,
    beta_t_unif = rt(n(), df = 3) * runif(n(), 0, 10),
    beta_t_cauchy = rt(n(), df = 3) * cauchy_sd,
    est_norm_unif = mu + beta_norm_unif + beta_hat,
    est_norm_cauchy = mu + beta_norm_cauchy + beta_hat,
    est_t_unif = mu + beta_t_unif + beta_hat,
    est_t_cauchy = mu + beta_t_cauchy + beta_hat,
    placebo_centered = rnorm(n(), mu, rcauchy(n(), 0, 1)),
    placebo_noncenter = mu + rnorm(n(), 0, 1) * rcauchy(n(), 0, 1)
  ) %>%
  gather(key = param, value = draw) %>%
  filter(between(draw, -1, 1) == TRUE,
         (param == "cauchy_sd" & draw <= 0) == FALSE,
         param != "beta_hat") %>%
  print()


ggplot(data = ppc_reeves, aes(x = draw)) +
  geom_histogram(bins = 20, boundary = -.1,
                 fill = "darkcyan", alpha = 0.4) +
  # geom_density(fill = "gray") +
  # xlim(c(-.2, .2)) +
  facet_wrap(~ param, scales = "free", nrow = 2) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

# separate figs for mu/sigma, pooling priors, and response estimates?



beepr::beep(2)

  rename(`Overall Policy\nTool Effect` = mu,
         `Specific Policy\nTool Effect (Residual)` = beta,
         `Estimated Policy\nTool Effect (Residual)` = beta_hat) %>%
  gather(key = param, value = value) %>%
  mutate(param = fct_relevel(param, "Overall Policy\nTool Effect", "Specific Policy\nTool Effect (Residual)")) %>%
  ggplot(aes(x = value)) +
    geom_histogram(boundary = 0, binwidth = .05,
                   fill = viridis::viridis(4)[3]) +
    facet_wrap(~ param) +
    xlim(c(-1, 1))

# density style
tibble(
  value = seq(-1, 2, by = .01),
  mu = dunif(value, -1, 1),
  sigma = ifelse(value > 0, 2 * dcauchy(value, 0, 1), NA)
) %>%
  gather(key = param, value = density, mu, sigma) %>%
  filter((param == "sigma" & value <= 0) == FALSE) %>%
  filter((param == "mu" & between(value, -1, 1) == FALSE) == FALSE) %>%  
  ggplot(aes(x = value, y = density)) +
    geom_ribbon(aes(ymin = 0, ymax = density),
                fill = "darkcyan", alpha = 0.5) +
    facet_wrap(~ param, scales = "free_x") +
    ylim(c(0, 1))