library(pacman)
p_load(fixest, tidyverse, broom)


est_did = feols(y ~ x1 + i(treat, period, 5) | id + period, base_did)

coefplot(est_did)



n_ind = 10
n_period = 11
n_obs = n_period*n_ind
event = 6

dat_iter = tibble(
  id = rep(1:n_ind, len = n_obs),
  time = rep(1:n_period, each = n_obs / n_period),
  post = ifelse(time >= event, 1, 0),
  treat = rep(0:1, length = n_obs),
  group = ifelse(treat == 1 & time >= event, 1,
                 ifelse(treat == 1 & time < event, 2,
                        ifelse(treat == 0 & time >= event, 3, 4))
  ),
  y = 2 + 2*(treat == 1) + (0.2*time) + 1.5*(treat == 1)*(post == 1) + rnorm(n_obs),
  z = 2 + 2*(treat == 1) + (0.2*time) + 1.5*(treat == 1)*(post == 1)*(time / event) + rnorm(n_obs)
) %>% panel(panel.id = ~id + time) %>% 
  arrange(id, time)



m1 = feols(data = dat_iter, y ~ i(treat, time, 5) | id + time)
fixest::coefplot(m1)

m2 = feols(data = dat_iter, z ~ i(treat, time, 5) | id + time)
fixest::coefplot(m2)
