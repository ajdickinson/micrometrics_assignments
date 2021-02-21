library(pacman)
p_load(tidyverse, ggplot2, fixest, broom, ggthemes)


es_sim = function(iter = 1, n_ind = 10, n_period = 11, event = 6, pre_treat = 5, post_treat = 5) {
  # event = event
  n_obs = n_period*n_ind
  dat_iter = tibble(
    iter = iter,
    event = event,
    id = rep(1:n_ind, len = n_obs),
    time = rep(1:n_period, each = n_obs / n_period),
    
    est_time = ifelse(time < event - pre_treat, -pre_treat,
                      ifelse(time > event + post_treat, post_treat, time - event)),
    
    post = ifelse(time >= event, 1, 0),
    lag1_post = ifelse(time == (event - 1) , 1, 0),
    lag2_post = ifelse(time == (event - 2) , 1, 0),
    treat = rep(0:1, length = n_obs),
    group = ifelse(treat == 1 & time >= event, 1,
                   ifelse(treat == 1 & time < event, 2,
                          ifelse(treat == 0 & time >= event, 3, 4))),
    para = 2 + 2*(treat == 1) + (0.2*time) + 1.5*(treat == 1)*(post == 1) + rnorm(n_obs),
    div = 2 + 2*(treat == 1) + (0.2*time) + 1.5*(treat == 1)*(post == 1)*(time / event) + rnorm(n_obs),
    ash = para - 1*(treat == 1)*(lag1_post == 1) - .5*(treat == 1)*(lag2_post == 1),
    ant = para + 1*(treat == 1)*(lag1_post == 1) + .5*(treat == 1)*(lag2_post == 1)
  ) %>% panel(panel.id = ~id + time) %>% 
    arrange(id, time)
  
  
  m_para = feols(data = dat_iter, para ~ i(treat, est_time, -1) | id + time) %>% tidy(conf.int = T) %>% 
    mutate(time = readr::parse_number(as.character(term))) %>%
    select(time, term, estimate, p.value, conf.low, conf.high) %>% 
    mutate(group = "para",
           iter_group = paste0("para", iter)) %>% 
    arrange(time)
  
  m_div = feols(data = dat_iter, div ~ i(treat, est_time, -1) | id + time) %>% tidy(conf.int = T) %>% 
    mutate(time = readr::parse_number(as.character(term))) %>%
    select(time, term, estimate, p.value, conf.low, conf.high) %>% 
    mutate(group = "div",
           iter_group = paste0("div", iter)) %>% 
    arrange(time)
  
  m_ash = feols(data = dat_iter, ash ~ i(treat, est_time, -1) | id + time) %>% tidy(conf.int = T) %>%
    mutate(time = readr::parse_number(as.character(term))) %>%
    select(time, term, estimate, p.value, conf.low, conf.high) %>%
    mutate(group = "ash",
           iter_group = paste0("ash", iter)) %>%
    arrange(time)
  
  m_ant = feols(data = dat_iter, ant ~ i(treat, est_time, -1) | id + time) %>% tidy(conf.int = T) %>%
    mutate(time = readr::parse_number(as.character(term))) %>%
    select(time, term, estimate, p.value, conf.low, conf.high) %>%
    mutate(group = "ant",
           iter_group = paste0("ant", iter)) %>%
    arrange(time)
   
  ret = rbind(m_para,m_div,m_ash,m_ant)
  
  return(ret)
}

sim_list <- map(1:20, es_sim, n_ind = 40, n_period = 11, event = 6, pre_treat = 5, post_treat = 5)
sim_df <- bind_rows(sim_list)

ggplot(data = sim_df, aes(x = time, y = estimate)) +
  geom_point(size = 0.5, alpha = 1) +
  geom_point(aes(x = -1, y = 0), size = 1, shape = 1) +
  # geom_line(aes(group = iter_group), alpha = 0.2) +
  geom_linerange(aes(ymin = conf.low, ymax = conf.high), position = position_dodge(width = 0.5), alpha = 0.1) +
  # geom_point(aes(y = conf.low, color = factor(zero)), size = 4, alpha= 0.75, shape = 95) +
  # geom_point(aes(y = conf.high, color = factor(zero)), size = 4, alpha= 0.75, shape = 95) +
  # geom_vline(xintercept = 5.5, linetype = "dashed", size = 0.5) +
  # geom_hline(yintercept = 0, linetype = "dashed", size = 0.5) +
  # scale_colour_manual(values = c("black", "red")) +
  ylim(-4.5, 4.5) +
  scale_x_continuous(breaks = seq(-5, 5, 1)) +
  theme_minimal() +
  facet_wrap(~group)





# 
# geom_smooth(data = sim_df %>% filter(group == "y", time < 5), aes(y = conf.low), se = F, method = "lm") + 
# geom_smooth(data = sim_df %>% filter(group == "y", time > 5), aes(y = conf.low), se = F, method = "lm") + 
# 
# geom_smooth(data = sim_df %>% filter(group == "y", time < 5), aes(y = conf.high), se = F, method = "lm") + 
# geom_smooth(data = sim_df %>% filter(group == "y", time > 5), aes(y = conf.high), se = F, method = "lm") + 
