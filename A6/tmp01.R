
library(pacman)
p_load(tidyverse, ggplot2, fixest, broom)


iter = 1
event = 51
n_ind = 10
n_period = 101
pre_treat = 25
post_treat = 25
ref = event - pre_treat - 1


n_obs = n_period*n_ind
dat_iter = tibble(
  iter = iter,
  event = event,
  id = rep(1:n_ind, len = n_obs),
  time = rep(1:n_period, each = n_obs / n_period),
  post = ifelse(time >= event, 1, 0),
  treat = rep(0:1, length = n_obs),
  group = ifelse(treat == 1 & time >= event, 1,
                 ifelse(treat == 1 & time < event, 2,
                        ifelse(treat == 0 & time >= event, 3, 4))),
  y = 2 + 2*(treat == 1) + (0.2*time) + 1.5*(treat == 1)*(post == 1) + rnorm(n_obs),
  z = 2 + 2*(treat == 1) + (0.2*time) + 1.5*(treat == 1)*(post == 1)*(time / event) + rnorm(n_obs)
) %>% panel(panel.id = ~id + time) %>% 
  mutate(ref_ = ref,
         pp_time = ifelse(time < event - pre_treat, event - pre_treat,
                    ifelse(time > event + post_treat, event + post_treat, time))) %>% 
  mutate(f_time = relevel(factor(pp_time), ref)) %>%
  arrange(id, time)




m1 = feols(data = dat_iter, y ~ i(treat, f_time, event[1]) | id + time) %>% tidy(conf.int = T) %>% 
  mutate(time = readr::parse_number(as.character(term))) %>%
  select(time, term, estimate, p.value, conf.low, conf.high) %>% 
  mutate(group = "y",
         iter_group = paste0("y", iter)) %>% 
  arrange(time)
