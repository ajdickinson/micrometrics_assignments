library(pacman)
p_load(tidyverse, ggplot2, fixest)



es_sim = function(iter = 1, n_ind = 10, n_period = 10, event = 5) {
  n_obs = n_period*n_ind
  dat_iter = tibble(
    iter = iter,
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
  ## run each model; treatment at i and c
  
  m1 = feols(data = dat_iter, y ~ i(treat, time, 5) | id + time) %>% tidy(conf.int = T) %>%
    select(term, estimate, conf.low, conf.high) %>% 
    add_row(term = "treat:time::5", estimate = 0, conf.low = 0, conf.high = 0) %>% 
    mutate(time = c(1:4, 6:10, 5),
           group = "y",
           iter_group = paste0("y", iter)) %>% 
    arrange(time) %>% 
    na.omit
  
  m2 = feols(data = dat_iter, z ~ i(treat, time, 5) | id + time) %>% tidy(conf.int = T) %>%
    select(term, estimate, conf.low, conf.high) %>% 
    add_row(term = "treat:time::5", estimate = 0, conf.low = 0, conf.high = 0) %>% 
    mutate(time = c(1:4, 6:10, 5),
           group = "z",
           iter_group = paste0("z", iter)) %>% 
    arrange(time) %>% 
    na.omit
  
  ret = rbind(m1,m2)
  
  return(ret)
}

sim_list <- map(1:100, es_sim)
sim_df <- bind_rows(sim_list)

ggplot(data = sim_df %>% filter(group == "y"), aes(x = time, y = estimate)) +
  geom_jitter(size = 0.1) +
  geom_smooth(data = sim_df %>% filter(group == "y", time < 5), aes(y = conf.low), se = F, method = "lm") + 
  geom_smooth(data = sim_df %>% filter(group == "y", time > 5), aes(y = conf.low), se = F, method = "lm") + 
  
  geom_smooth(data = sim_df %>% filter(group == "y", time < 5), aes(y = conf.high), se = F, method = "lm") + 
  geom_smooth(data = sim_df %>% filter(group == "y", time > 5), aes(y = conf.high), se = F, method = "lm") + 
  
  geom_vline(xintercept = 6, linetype = "dashed") +
  scale_x_continuous(breaks = seq(0, 10, 1))
