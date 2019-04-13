# ----------------------------------------------------
#   Bayesian replication of Hall 2015: 
#   (What happens when extremists win primaries?)
# ----------------------------------------------------

# workhorse
library("here")
library("magrittr")
library("tidyverse")
library("scales")
library("broom")
library("latex2exp")

# commarobust()
library("estimatr")

# bayes
library("rstan")
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)
library("tidybayes")
library("ggmcmc")
library("brms")

# graphics theme
theme_set(
  ggthemes::theme_base(base_family = "Source Sans Pro", base_size = 14) + 
  theme(plot.background = element_blank(), 
        axis.ticks = element_line(lineend = "square"), 
        axis.ticks.length = unit(0.25, "lines"))
)

# party colors if necessary
dblue <- "#259FDD"
rred <- "#FC5E47"


# ----------------------------------------------------
#   Pure Replication
# ----------------------------------------------------

# ---- data -----------------------

hall_raw <- 
  haven::read_dta(here("data", "hall-extremists", "primary_analysis.dta")) %>%
  rename(dv_vote = dv) %>%
  print()

# ---- cutoff parameters -----------------------

# vote shares near cutoff (for "local linear")
(local_margin <- 0.05)
(min_ideo_distance <- hall_raw %$% median(absdist))

# checking N obs
hall_raw %>% filter(absdist > min_ideo_distance)
hall_raw %>% filter(margin < local_margin, absdist > min_ideo_distance)




# ---- shape data for purrr estimation -----------------------

# elongate DVs, 
# - informative labels
# - indicator for unanimous outcomes
hall_long <- hall_raw %>%
  mutate(unanimous = dv_vote %in% c(0, 1)) %>%
  gather(key = dv_name, value = dv_value, dv_vote, dv_win) %>%
  print()

# functional forms
local_formula <- dv_value ~ treat*rv
cubic_formula <- dv_value ~ treat + rv + rv2 + rv3

# estimate models
# - also create nonunanimous data
# - linear and cubic specifications (note filtering for each)
# - gather and robustify
# - gather and tidy/glance/augment
# - glance, tidy
nonbayes_fits <- hall_long %>%
  group_by(dv_name) %>%
  nest(.key = full_data) %>%
  mutate(nonunanimous_data = 
           map(full_data, filter, unanimous == FALSE)) %>%
  gather(key = data_scope, value = data, ends_with("_data"))  %>%
  mutate(
    local_linear = map(data, ~ .x %>% 
                               filter(absdist > min_ideo_distance) %>% 
                               filter(margin < local_margin) %>% 
                               lm(local_formula, data = .)),
    cubic = map(data, ~ .x %>% 
                        filter(absdist > min_ideo_distance) %>% 
                        lm(cubic_formula, data = .))
  ) %>%
  gather(key = specification, value = std_model, local_linear, cubic) %>%
  mutate(robust_model = map(std_model, commarobust)) %>%
  gather(key = std_err, value = model, std_model, robust_model) %>%
  mutate(glance_model = map(model, ~ try(glance(.x))),
         tidy_model = map(model, ~ try(tidy(.x, conf.int = TRUE))),
         augment_model = map(model, ~ try(augment(.x, conf.int = TRUE)))
  ) %>%
  print()

# quick view coefs
nonbayes_fits %>%
  unnest(tidy_model) %>%
  mutate_if(is.numeric, round, 2)



# plot treatment effects
nonbayes_treatments <- nonbayes_fits %>%
  unnest(tidy_model) %>%
  filter(term == "treat") %>%
  print()


ggplot(nonbayes_treatments, aes(x = specification, y = estimate)) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high,
                      color = std_err, shape = data_scope),
                  fill = "white",
                  position = position_dodge(width = 0.25)) +
  facet_wrap(~ dv_name,
             labeller = 
               as_labeller(c("dv_vote" = "Vote Share", 
                             "dv_win" = "Win Probability"))) +
  coord_flip() +
  scale_y_continuous(labels = percent) +
  scale_shape_manual(values = c("full_data" = 16,
                                    "nonunanimous_data" = 21))




# cubic?
nonbayes_fits %>%
  unnest(tidy_model) %>%
  mutate_if(is.numeric, round, 2) %>%
  filter(specification == "cubic") %>%
  filter(str_detect(term, "rv") == FALSE)


# vote
nonbayes_fits %>%
  unnest(tidy_model) %>%
  mutate_if(is.numeric, round, 2) %>%
  filter(dv_name == "dv_vote") %>%
  filter(data_scope == "full_data") %>%
  filter(std_err == "std_model") %>%
  filter(str_detect(term, "rv") == FALSE)


# win
nonbayes_fits %>%
  unnest(tidy_model) %>%
  mutate_if(is.numeric, round, 2) %>%
  filter(dv_name == "dv_win") %>%
  filter(data_scope == "full_data") %>%
  filter(std_err == "std_model") %>%
  filter(str_detect(term, "rv") == FALSE)



# ---- RDD plot: vote -----------------------

nonbayes_fits %>%
  filter(dv_name == "dv_vote",
         data_scope == "nonunanimous_data",
         specification == "local_linear",
         std_err == "robust_model") %>%
  unnest(data) %>%
  filter(absdist > min_ideo_distance,
         margin < local_margin) %>%
  ggplot(aes(x = rv, y = dv_value)) +
    geom_vline(xintercept = c(local_margin, 0, -local_margin), 
               linetype = "dotted") +
    geom_hline(yintercept = c(0, 1), linetype = "dotted") +
    # geom_ribbon(data = hyp_lines,
    #             aes(x = low, y = NULL, 
    #                 ymin = alpha_0 + low*beta_0_low, 
    #                 ymax = alpha_0 + low*beta_0_high),
    #             color = NA, fill = "steelblue") +
    # geom_ribbon(data = hyp_lines,
    #             aes(x = high, y = NULL, 
    #                 ymin = alpha_1 + high*beta_1_low, 
    #                 ymax = alpha_1 + high*beta_1_high),
    #             color = NA, fill = "steelblue") +
    geom_smooth(method = "lm", aes(group = treat),
                color = "black", fill = "steelblue") +
    geom_point(size = 2, shape = 21, fill = "gray") +
    scale_y_continuous(breaks = seq(0, 1, .2),
                       labels = percent) + 
    scale_x_continuous(labels = percent) +
    labs(x = "Extremist Candidate Margin in Primary Election",
         y = "Probability that Extremist's Party\nWins General Election") +
    # annotate("text", family = "Minion Pro", label = TeX("$\\omega$ (upper)"),
    #          x = local_margin - .0075, y = .2) +
    # annotate("text", family = "Minion Pro", label = TeX("$-\\omega$ (lower)"),
    #          x = -local_margin + .0075, y = .2) +
    NULL



# ---- RDD plot: win -----------------------

bw_lines <- 
  tribble(~ bw, ~ greek, 
          -0.05, TeX("$-\\omega$"), 
          .05, TeX("$\\omega$")) %>%
  print()


hyp_lines <- 
  tibble(alpha_0 = 0.6, alpha_1 = 0.35,
         beta_0_low = -alpha_0 / -local_margin,
         beta_0_high = (1-alpha_0) / -local_margin,
         beta_1_low = -alpha_1 / local_margin,
         beta_1_high = (1-alpha_1) / local_margin,) %>%
  crossing(tibble(low = seq(-local_margin, 0, .001), 
                  high = seq(0, local_margin, .001))
  ) %>%
  print()


nonbayes_fits %>%
  filter(dv_name == "dv_win",
         data_scope == "full_data",
         specification == "local_linear",
         std_err == "robust_model") %>%
  unnest(data) %>%
  filter(absdist > min_ideo_distance,
         margin < local_margin) %>%
  ggplot(aes(x = rv, y = dv_value)) +
    geom_vline(xintercept = c(local_margin, 0, -local_margin), 
               linetype = "dotted") +
    geom_hline(yintercept = c(0, 1), linetype = "dotted") +
    # geom_ribbon(data = hyp_lines,
    #             aes(x = low, y = NULL, 
    #                 ymin = alpha_0 + low*beta_0_low, 
    #                 ymax = alpha_0 + low*beta_0_high),
    #             color = NA, fill = "steelblue") +
    # geom_ribbon(data = hyp_lines,
    #             aes(x = high, y = NULL, 
    #                 ymin = alpha_1 + high*beta_1_low, 
    #                 ymax = alpha_1 + high*beta_1_high),
    #             color = NA, fill = "steelblue") +
    geom_smooth(method = "lm", aes(group = treat),
                color = "black", fill = "steelblue") +
    geom_point(size = 2, shape = 21, fill = "gray") +
    scale_y_continuous(breaks = seq(0, 1, .2),
                       labels = percent) + 
    scale_x_continuous(labels = percent) +
    labs(x = "Extremist Candidate Margin in Primary Election",
         y = "Probability that Extremist's Party\nWins General Election") +
    annotate("text", family = "Minion Pro", label = TeX("$\\omega$ (upper)"),
             x = local_margin - .0075, y = .2) +
    annotate("text", family = "Minion Pro", label = TeX("$-\\omega$ (lower)"),
             x = -local_margin + .0075, y = .2) 



# ----------------------------------------------------
#   BRM version
# ----------------------------------------------------

hall_brm <- hall_raw %>%
  mutate(
    control = as.numeric(treat == 0), 
    control_rv = case_when(treat == 0 ~ 100 * rv, 
                           treat == 1 ~ 0), 
    treat_rv = case_when(treat == 1 ~ 100 * rv, 
                         treat == 0 ~ 0)
  ) %>%
  print()



# gaussian priors
get_prior(dv_win ~ 0 + control + treat + control_rv + treat_rv,
          filter(hall_brm, margin < local_margin,
                 absdist > min_ideo_distance))

# logit
get_prior(dv_win ~ 0 + control + treat + control_rv + treat_rv,
          family = bernoulli,
          filter(hall_brm, margin < local_margin,
                 absdist > min_ideo_distance))



win_lm <- lm(dv_win ~ 0 + control + treat + control_rv + treat_rv,
   data = filter(hall_brm, margin < local_margin,
                 absdist > min_ideo_distance))

win_logit_mle <- 
  glm(dv_win ~ 0 + control + treat + control_rv + treat_rv, 
      data = filter(hall_brm, margin < local_margin, 
                    absdist > min_ideo_distance),
      family = "binomial")


tidy(win_lm)
tidy(win_logit_mle)


win_flat <- brm(dv_win ~ 0 + control + treat + control_rv + treat_rv, 
                data = hall_brm %>% 
                       filter(margin < local_margin, 
                              absdist > min_ideo_distance))

win_trunc <- 
  brm(
    dv_win ~ 0 + control + treat + control_rv + treat_rv, 
    prior = c(set_prior("uniform(0, 1)", 
                        class = "b", 
                        coef = c("control", "treat")), 
              set_prior("student_t(3, 0, 1)", class = "sigma")),
      data = hall_brm %>% filter(margin < local_margin, 
                                 absdist > min_ideo_distance)
  )



win_logit_flat <- 
  brm(
    dv_win ~ 0 + control + treat + control_rv + treat_rv, 
    family = bernoulli,
    data = hall_brm %>% filter(margin < local_margin, 
                               absdist > min_ideo_distance)
  )


win_logit <- 
  brm(
    dv_win ~ 0 + control + treat + control_rv + treat_rv, 
    family = bernoulli,
    prior = c(set_prior("normal(0, 1.5)", class = "b",
                        coef = c("control", "treat")),
              set_prior("normal(0, 1)", class = "b",
                        coef = c("control_rv", "treat_rv"))),
      data = hall_brm %>% filter(margin < local_margin, 
                                 absdist > min_ideo_distance)
  )


win_logit_prior <- 
  brm(
    dv_win ~ 0 + control + treat + control_rv + treat_rv, 
    family = bernoulli,
    prior = c(set_prior("normal(0, 1.5)", class = "b",
                        coef = c("control", "treat")),
              set_prior("normal(0, 1)", class = "b",
                        coef = c("control_rv", "treat_rv"))),
      data = hall_brm %>% filter(margin < local_margin, 
                                 absdist > min_ideo_distance),
      sample_prior = "only"
  )




win_lm %>%
  saveRDS(here("data", "estimates", "hall", "brm-win_lm.RDS"))
win_flat %>%
  saveRDS(here("data", "estimates", "hall", "brm-win_flat.RDS"))
win_trunc %>%
  saveRDS(here("data", "estimates", "hall", "brm-win_trunc.RDS"))
win_logit %>%
  saveRDS(here("data", "estimates", "hall", "brm-win_logit.RDS"))
win_logit_mle %>%
  saveRDS(here("data", "estimates", "hall", "brm-win_logit_mle.RDS"))


# ---- means -----------------------

win_lm %>%
  tidy() %$%
  {estimate[2] - estimate[1]}

win_logit_mle %>%
  tidy() %$%
  {plogis(estimate[2]) - plogis(estimate[1])}

win_flat %>%
  tidy() %$%
  {estimate[2] - estimate[1]}

win_trunc %>%
  tidy() %>%
  (function(x) (x$estimate[2]) - (x$estimate[1]))

win_logit_flat %>%
  tidy() %>%
  (function(x) plogis(x$estimate[2]) - plogis(x$estimate[1]))

win_logit %>%
  tidy() %>%
  (function(x) plogis(x$estimate[2]) - plogis(x$estimate[1]))

# ---- variances -----------------------
win_lm %>% tidy() 
win_flat %>% tidy()


list(win_flat = win_flat,
     win_trunc = win_trunc, 
     win_logit_flat = win_logit_flat, 
     win_logit = win_logit) %>%
  tibble(spec = names(.),
         brm_fit = .) %>%
  group_by(spec) %>% 
  mutate(wide = map(brm_fit, spread_draws, b_control, b_treat)) %>%
  unnest(wide) %>%
  mutate(
    b_control = ifelse(str_detect(spec, "logit"), plogis(b_control), b_control),
    b_treat = ifelse(str_detect(spec, "logit"), plogis(b_treat), b_treat),
    diff = b_treat - b_control) %>%
  gather(key = param, value = value, b_control, b_treat, diff) %>%
  filter(spec %in% c(
                     # "win_logit_flat", 
                     "win_trunc",
                     "win_flat"
                     )) %>%
  ggplot(aes(x = value)) +
    facet_wrap(~ param, scales = "free") +
    geom_histogram(aes(fill = spec),
                   position = "identity",
                   alpha = 0.7,
                   binwidth = .05,
                   boundary = 1,
                   color = "black", size = 0.25) +
    scale_fill_manual(values = c("win_flat" = purp, "win_trunc" = yel)) +
    scale_x_continuous(breaks = seq(-1, 1, .5))
    # viridis::scale_fill_viridis(discrete = TRUE, option = "magma", end = 0, begin = 0.)









# ----------------------------------------------------
#   Back out win probability?
# ----------------------------------------------------



