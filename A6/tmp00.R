library(pacman)
p_load(tidyverse, ggplot2, fixest, broom)



es_sim = function(iter = 1, n_ind = 10, n_period = 11, event = 6) {
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
    mutate(f_time = relevel(factor(time), ref = ref)) %>% 
    arrange(id, time)
  ## run each model; treatment at i and c
  
  m1 = feols(data = dat_iter, y ~ i(treat, time, event) | id + time) %>% tidy(conf.int = T) %>%
    select(term, estimate, conf.low, conf.high) %>% 
    add_row(term = paste0("treat:time::",(event - 1)), estimate = 0, conf.low = 0, conf.high = 0) %>% 
    mutate(group = "y",
           iter_group = paste0("y", iter)) %>% 
    arrange(time) %>% 
    na.omit
  
  m2 = feols(data = dat_iter, z ~ i(treat, time, event) | id + time) %>% tidy(conf.int = T) %>%
    select(term, estimate, conf.low, conf.high) %>% 
    add_row(term = paste0("treat:time::",(event - 1)), estimate = 0, conf.low = 0, conf.high = 0) %>% 
    mutate(group = "z",
           iter_group = paste0("z", iter)) %>% 
    arrange(time) %>% 
    na.omit
  
  ret = rbind(m1,m2)
  
  return(ret)
}

## create time var
## create zero var
##   zero = ifelse((conf.low <= 0 & time > event) | (conf.low >= 0 & time < (event - 1)), 1, 0)

sim_list <- map(1, es_sim, n_ind = 20)
sim_df <- bind_rows(sim_list)

ggplot(data = sim_df %>% filter(group == "y"), aes(x = time, y = estimate)) +
  geom_point(size = 1, alpha = 0.4) +
  # geom_line(aes(group = iter_group), alpha = 0.2) +
  geom_linerange(aes(ymin = conf.low, ymax = conf.high), position = position_dodge(width = 0.5), alpha = 1) +
  geom_point(aes(y = conf.low, color = factor(zero)), size = 4, alpha= 0.75, shape = 95) +
  geom_point(aes(y = conf.high, color = factor(zero)), size = 4, alpha= 0.75, shape = 95) +
  scale_x_continuous(breaks = seq(0, 11, 1)) +
  geom_vline(xintercept = 5.5, linetype = "dashed", size = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed", size = 0.5) +
  scale_colour_manual(values = c("black", "red"))
