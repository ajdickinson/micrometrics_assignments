library(pacman)
p_load(tidyverse,patchwork,kableExtra,lfe,lmtest,xtable,
       did,magrittr,ggthemes,bacondecomp,multcomp,fastDummies,
       ggforce)


sim1 = function(...) {
  ## generate unit specific variation
  unit <- tibble(
    unit = 1:1000,
    unit_fe = rnorm(1000, 0, 1),
    state = sample(rep(1:50, 20), 1000, replace = FALSE),
    group = ifelse(state %in% 1:25, 1, 2),
    treated = ifelse(group == 2, 1, 0),
    # average yearly treatment effect (by group) [\beta = 2]
    beta_hat = ifelse(group == 2, 2, 0),
    ) %>%
    rowwise() %>% 
  mutate(beta = ifelse(group == 2, rnorm(1, beta_hat, 0.5), 0)) %>% 
  ungroup()

  ## generate year specific variation
  year <- tibble(
    year = 1995:2020,
    year_fe = rnorm(26, 0, 1)
  )

  ## Put them together
  dat_iter = crossing(unit, year) %>% 
    mutate(e = rnorm(nrow(.), 0, 1),
           treat = ifelse(treated == 1 & year >= 2005, 1, 0),
           tau = ifelse(treat == 1, beta, 0),
           y = unit_fe + year_fe + tau + e)
  
  return(dat_iter)
}



sim2 = function(...) {
  ## generate unit specific variation
  unit <- tibble(
    unit = 1:1000,
    unit_fe = rnorm(1000, 0, 1),
    state = sample(rep(1:50, 20), 1000, replace = FALSE),
    group = ifelse(state %in% 1:25, 1, 2),
    treated = ifelse(group == 2, 1, 0),
    # average yearly treatment effect (by group) [\beta = 2]
    beta_hat = ifelse(group == 2, 0.5, 0),
  ) %>%
    rowwise() %>% 
    mutate(beta = rnorm(1, beta_hat, 0.5), 0) %>% 
    ungroup()
  
  ## generate year specific variation
  year <- tibble(
    year = 1995:2020,
    year_fe = rnorm(26, 0, 1)
  )
  
  ## slap them together
  dat_iter = crossing(unit, year) %>% 
    mutate(e = rnorm(nrow(.), 0, 1),
           treat = ifelse(treated == 1 & year >= 2005, 1, 0),
           tau = ifelse(treat == 1, beta, 0)) %>% 
  ## Generate time varying dependent variable
    group_by(unit) %>% 
    mutate(cumtau = cumsum(tau)) %>% 
    mutate(y = unit_fe + year_fe + cumtau + e)

    return(dat_iter)
}



sim3 = function(...) {
  ## generate unit specific variation
  unit <- tibble(
    unit = 1:1000,
    unit_fe = rnorm(1000, 0, 1),
    state = sample(rep(1:50, 20), 1000, replace = FALSE),
    group = case_when(
      state %in% 1:17 ~ 2000,
      state %in% 18:35 ~ 2008,
      state %in% 35:50 ~ 2016
    ),
    beta_hat = case_when(
      group == 2000 ~ 3,
      group == 2008 ~ 3,
      group == 2016 ~ 3
    )) %>%
    rowwise() %>% 
    mutate(beta = rnorm(1, beta_hat, 0.5), 0) %>% 
    ungroup()
  
  ## generate year specific variation
  year <- tibble(
    year = 1995:2020,
    year_fe = rnorm(26, 0, 1)
  )
  
  ## Put them together
  dat_iter = crossing(unit, year) %>% 
    mutate(e = rnorm(nrow(.), 0, 1),
           treat = ifelse(year >= group, 1, 0),
           tau = ifelse(treat == 1, beta, 0),
           y = unit_fe + year_fe + tau + e)
  
  return(dat_iter)
}


sim4 = function(...) {
  ## generate unit specific variation
  unit <- tibble(
    unit = 1:1000,
    unit_fe = rnorm(1000, 0, 1),
    state = sample(rep(1:50, 20), 1000, replace = FALSE),
    group = case_when(
      state %in% 1:17 ~ 2000,
      state %in% 18:35 ~ 2008,
      state %in% 35:50 ~ 2016
    ),
    beta_hat = case_when(
      group == 2000 ~ 5,
      group == 2008 ~ 3,
      group == 2016 ~ 1
    )) %>%
    rowwise() %>% 
    mutate(beta = rnorm(1, beta_hat, 0.5), 0) %>% 
    ungroup()
  
  ## generate year specific variation
  year <- tibble(
    year = 1995:2020,
    year_fe = rnorm(26, 0, 1)
  )
  
  ## Put them together
  dat_iter = crossing(unit, year) %>% 
    mutate(e = rnorm(nrow(.), 0, 1),
           treat = ifelse(year >= group, 1, 0),
           tau = ifelse(treat == 1, beta, 0),
           y = unit_fe + year_fe + tau + e)
  
  return(dat_iter)
}