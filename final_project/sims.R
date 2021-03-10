library(pacman)
p_load(tidyverse,patchwork,kableExtra,lfe,lmtest,xtable,
       did,magrittr,ggthemes,bacondecomp,multcomp,fastDummies,
       ggforce, hrbrthemes)


red_pink <- "#e64173"
carolina_blue <- "#99badd"
turquoise <- "#20B2AA"
azure <- "#007fff"
columbia_blue <- "#9bddff"
blue_grey <- "#6699cc"
cool_grey <- "#8c92ac"
cool_black <- "#002e63"
orange <- "#FFA500"
red <- "#fb6107"
blue <- "#3b3b9a"
green <- "#38c57f"
grey_light <- "grey70"
grey_mid <- "grey50"
grey_dark <- "grey20"
purple <- "#6A5ACD"
slate <- "#314f4f"
rmd_green <- "#73d7ad"
rmd_pink <- "#d7739d"
uo_green <- "#007A33"
grant_red <- "#AA0000"
coral_red <- "#ff4040"

## SIM 01 ######################################################################
#### sim function

sim1 = function(...) {
  ## generate unit specific variation
  unit <- tibble(
    unit = 1:1000,
    unit_fe = rnorm(1000, 0, 0.5),
    state = sample(rep(1:50, 20), 1000, replace = FALSE),
    group = ifelse(state %in% 1:25, 1, 2),
    treated = ifelse(group == 2, 1, 0),
    # average yearly treatment effect (by group) [\beta = 2]
    beta_hat = ifelse(group == 2, 2, 0),
    ) %>%
    rowwise() %>% 
  mutate(beta = ifelse(group == 2, rnorm(1, beta_hat, 0.2), 0)) %>% 
  ungroup()

  ## generate year specific variation
  year <- tibble(
    year = 1995:2020,
    year_fe = rnorm(26, 0, 0.5)
  )

  ## Put them together
  dat_iter = crossing(unit, year) %>% 
    mutate(e = rnorm(nrow(.), 0, 0.5),
           treat = ifelse(treated == 1 & year >= 2005, 1, 0),
           tau = ifelse(treat == 1, beta, 0),
           y = unit_fe + year_fe + tau + e)
  
  return(dat_iter)
}

#### sim

data1 = sim1()

#### plot

g0 = ggplot(data = data1, aes(x = year, y = y, group = unit)) + 
  geom_line(alpha = 0.1, size = 0.25, color = grey_mid) +
  theme_ipsum() +
  labs(title = "Simulation 01",
       subtitle = expression(paste("Single treatment period + homogenous treatment effect (", tau, " = 2)")),
       caption = "Treatment turns on in 2005",
       x = "Year",
       y = "Y",
       color = "Treatment") +
  geom_line(data = data1 %>% 
              group_by(treated, year) %>% 
              summarise(mean_y = mean(y)),
            aes(x = year, y = mean_y, group = factor(treated), color = factor(treated)),
            size = 1.25) +
  geom_vline(xintercept = 2005, linetype = "dashed", size = 0.25) +
  scale_color_manual(values = c(grey_dark, azure)) +
  scale_y_continuous(breaks = seq(-4,8,2))
  
g0

## SIM 02 ######################################################################

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

#### sim

# data2 = sim2()
  # 
  # #### plot
  # 
  # g1 = ggplot(data = data2, aes(x = year, y = y, group = unit)) + 
  #   geom_line(alpha = 0.1, size = 0.25, color = grey_mid) +
  #   theme_ipsum() +
  #   labs(title = "Simulation 01",
  #        subtitle = expression(paste("Single treatment period + homogenous treatment effect (", tau, " = 2)")),
  #        caption = "Treatment turns on in 2005",
  #        x = "Year",
  #        y = "Y",
  #        color = "Treatment")
  # 
  # g1
  # 
  # +
  #   geom_line(data = data2 %>% 
  #               group_by(treated, year) %>% 
  #               summarise(mean_y = mean(y)),
  #             aes(x = year, y = mean_y, group = factor(treated), color = factor(treated)),
  #             size = 1.25) +
  #   geom_vline(xintercept = 2005, linetype = "dashed", size = 0.25) +
  #   scale_color_manual(values = c(grey_dark, azure)) +
  #   scale_y_continuous(breaks = seq(-4,8,2))
  # 
  # g0


## SIM 03 ######################################################################

sim3 = function(...) {
  ## generate unit specific variation
  unit <- tibble(
    unit = 1:1000,
    unit_fe = rnorm(1000, 0, 0.5),
    state = sample(rep(1:50, 20), 1000, replace = FALSE),
    group = case_when(
      state %in% 1:17 ~ 2000,
      state %in% 18:35 ~ 2008,
      state %in% 35:50 ~ 2016
    ),
    beta_hat = case_when(
      group == 2000 ~ 2,
      group == 2008 ~ 2,
      group == 2016 ~ 2
    )) %>%
    rowwise() %>% 
    mutate(beta = rnorm(1, beta_hat, 0.2), 0) %>% 
    ungroup()
  
  ## generate year specific variation
  year <- tibble(
    year = 1995:2020,
    year_fe = rnorm(26, 0, 0.5)
  )
  
  ## Put them together
  dat_iter = crossing(unit, year) %>% 
    mutate(e = rnorm(nrow(.), 0, 0.5),
           treat = ifelse(year >= group, 1, 0),
           tau = ifelse(treat == 1, beta, 0),
           y = unit_fe + year_fe + tau + e)
  
  return(dat_iter)
}

#### sim

data3 = sim3()

#### plot

g3 = ggplot(data = data3, aes(x = year, y = y, group = unit)) + 
  geom_line(alpha = 0.1, size = 0.25, color = grey_mid) +
  theme_ipsum() +
  labs(title = "Simulation 03",
       subtitle = expression(paste("Staggered treatment periods + homogenous treatment effect (", tau, " = 2)")),
       caption = "Treatment is staggered between 2000, 2008, 2016. All units are treated after 2016.",
       x = "Year",
       y = "Y",
       color = "Treatment") +
  geom_line(data = data3 %>% 
              group_by(group, year) %>% 
              summarise(mean_y = mean(y)),
            aes(x = year, y = mean_y, group = factor(group), color = factor(group)),
            size = 1.25) +
  geom_vline(xintercept = c(1999.5, 2007.5, 2015.5), linetype = "dashed", size = 0.25) +
  scale_color_manual(values = c(cool_black, azure, blue_grey)) +
  scale_y_continuous(breaks = seq(-4,8,2))

g3

## SIM 04 ######################################################################

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

#### sim

data4 = sim4()

#### plot

g4 = ggplot(data = data4, aes(x = year, y = y, group = unit)) + 
  geom_line(alpha = 0.1, size = 0.25, color = grey_mid) +
  theme_ipsum() +
  labs(title = "Simulation 04",
       subtitle = expression(paste("Staggered treatment periods + constant treatment effect that change over time (", tau, " = 5, 3, 1)")),
       caption = "Treatment is staggered between 2000, 2008, 2016. All units are treated after 2016.",
       x = "Year",
       y = "Y",
       color = "Treatment") +
  geom_line(data = data4 %>% 
              group_by(group, year) %>% 
              summarise(mean_y = mean(y)),
            aes(x = year, y = mean_y, group = factor(group), color = factor(group)),
            size = 1.25) +
  geom_vline(xintercept = c(1999.5, 2007.5, 2015.5), linetype = "dashed", size = 0.25) +
  scale_color_manual(values = c(cool_black, azure, columbia_blue)) +
  scale_y_continuous(breaks = seq(-4,8,2))

g4
