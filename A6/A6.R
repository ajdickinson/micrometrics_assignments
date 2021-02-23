library(pacman)
p_load(tidyverse, ggplot2, fixest, broom, ggthemes)


es_sim = function(iter = 1, n_ind = 10, n_period = 11, event = 6, pre_treat = 5, post_treat = 5) {
  n_obs = n_period*n_ind
  dat_iter = tibble(
    iter = iter,
    event = event,
    id = rep(1:n_ind, len = n_obs),
    time = rep(1:n_period, each = n_obs / n_period),
    est_time = case_when(
          time < event - pre_treat ~ -pre_treat,
          time > event + post_treat ~ post_treat,
          TRUE ~ time - event
        ),
    post = ifelse(time >= event, 1, 0),
    lag1_post = ifelse(time == (event - 1) , 1, 0),
    lag2_post = ifelse(time == (event - 2) , 1, 0),
    treat = rep(0:1, length = n_obs),
    group = case_when(
          treat == 1 & time >= event ~ 1,
          treat == 1 & time <event ~ 2,
          treat == 0 & time >= event ~ 3,
          TRUE ~ 4
        ),
    ## Generate outcome variables
    ## parallel trends
    para = 2 + 2*(treat == 1) + (0.2*time) + 1.5*(treat == 1)*(post == 1) + rnorm(n_obs),
    ## not parallel trends
    npara = para - (0.2*time)*(treat == 0),
    ## divergent trends following treatment
    div = 2 + 2*(treat == 1) + (0.2*time) + 1.5*(treat == 1)*(post == 1)*(time / event)^2 + rnorm(n_obs),
    ## ashenfelter dip - selection into treatment
    ash = para - 1*(treat == 1)*(lag1_post == 1) - .5*(treat == 1)*(lag2_post == 1),
    ## anticipation of treatment
    ant = para + 1*(treat == 1)*(lag1_post == 1) + 1*(treat == 1)*(lag2_post == 1)
  ) %>% panel(panel.id = ~id + time) %>% 
    arrange(id, time)
  
  ## Estimate the five models 
  
  m_para = feols(data = dat_iter, para ~ i(treat, est_time, -1) | id + time) %>% tidy(conf.int = T) %>% 
    mutate(time = readr::parse_number(as.character(term))) %>%
    select(time, term, estimate, p.value, conf.low, conf.high) %>% 
    mutate(group = "Parallel trends",
           iter_group = paste0("Parallel trends", iter)) %>% 
    arrange(time)
  
  m_npara = feols(data = dat_iter, npara ~ i(treat, est_time, -1) | id + time) %>% tidy(conf.int = T) %>% 
    mutate(time = readr::parse_number(as.character(term))) %>%
    select(time, term, estimate, p.value, conf.low, conf.high) %>% 
    mutate(group = "Not parallel trends",
           iter_group = paste0("Not parallel trends", iter)) %>% 
    arrange(time)
  
  m_div = feols(data = dat_iter, div ~ i(treat, est_time, -1) | id + time) %>% tidy(conf.int = T) %>% 
    mutate(time = readr::parse_number(as.character(term))) %>%
    select(time, term, estimate, p.value, conf.low, conf.high) %>% 
    mutate(group = "Divergent post treatment trends",
           iter_group = paste0("Divergent post treatment trends", iter)) %>% 
    arrange(time)
  
  m_ash = feols(data = dat_iter, ash ~ i(treat, est_time, -1) | id + time) %>% tidy(conf.int = T) %>%
    mutate(time = readr::parse_number(as.character(term))) %>%
    select(time, term, estimate, p.value, conf.low, conf.high) %>%
    mutate(group = "Ashenfelter dip",
           iter_group = paste0("Ashenfelter dip", iter)) %>%
    arrange(time)
  
  m_ant = feols(data = dat_iter, ant ~ i(treat, est_time, -1) | id + time) %>% tidy(conf.int = T) %>%
    mutate(time = readr::parse_number(as.character(term))) %>%
    select(time, term, estimate, p.value, conf.low, conf.high) %>%
    mutate(group = "Anticipation of treatment",
           iter_group = paste0("Anticipation of treatment", iter)) %>%
    arrange(time)
   
  ret = rbind(m_para,m_npara,m_div,m_ash,m_ant)
  
  return(ret)
}

n_ind = 40
n_period = 110
event = 70
pre_treat = 5
post_treat = 10

sim_list <- map(1:20, es_sim, n_ind = n_ind, n_period = n_period,
                event = event, pre_treat = pre_treat, post_treat = post_treat)
sim_df <- bind_rows(sim_list)

ggplot(data = sim_df, aes(x = time, y = estimate)) +
  geom_point(size = 0.5, alpha = 1) +
  geom_point(aes(x = -1, y = 0), size = 1, shape = 1) +
  # geom_line(aes(group = iter_group), alpha = 0.2, size = 0.5, color = "grey50") +
  # geom_line(aes(x = time, y = estimate),alpha = 1) +
  geom_linerange(aes(ymin = conf.low, ymax = conf.high), position = position_dodge(width = 0.5), alpha = 0.1) +
  # geom_point(aes(y = conf.low, color = factor(zero)), size = 4, alpha= 0.75, shape = 95) +
  # geom_point(aes(y = conf.high, color = factor(zero)), size = 4, alpha= 0.75, shape = 95) +
  # geom_vline(xintercept = 5.5, linetype = "dashed", size = 0.5) +
  # geom_hline(yintercept = 0, linetype = "dashed", size = 0.5) +
  # scale_colour_manual(values = c("black", "red")) +
  geom_hline(yintercept = 1.5, linetype = "dashed", color = "red", alpha = 0.3) +
  ylim(min(sim_df$conf.low), max(sim_df$conf.high)) +
  scale_x_continuous(breaks = seq(-pre_treat, post_treat, 1)) +
  theme_minimal() +
  facet_wrap(~group)

