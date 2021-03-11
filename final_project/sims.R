library(pacman)
p_load(tidyverse,patchwork,kableExtra,fixest,lmtest,xtable,
       did,magrittr,ggthemes,bacondecomp,multcomp,fastDummies,
       ggforce, hrbrthemes, tictoc)


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


# do simulations, estimate ATT and plot ---------------------------------
# make function to do this by data type 
dosims <- function(i, fun) {
  # make data from function
  dt <- fun()
  # estimate model
  feols(y ~ treat | unit + year , cluster = ~state, data = dt) %>% 
    broom::tidy(conf.int = TRUE) %>% 
    dplyr::select(estimate) %>% 
    mutate(sim = i)
}

## SIM 01 ######################################################################
#### sim function

sim01 = function(...) {
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

#### sim one

data01 = sim01()

  ## plot

  g10 = ggplot(data = data01, aes(x = year, y = y, group = unit)) + 
    geom_line(alpha = 0.1, size = 0.25, color = grey_mid) +
    theme_ipsum() +
    labs(title = "Simulation 01",
         subtitle = expression(paste("Single treatment period + homogenous treatment effect (", tau, " = 2)")),
         caption = "Treatment turns on in 2005",
         x = "Year",
         y = "Y",
         color = "Treatment") +
    geom_line(data = data01 %>% 
                group_by(treated, year) %>% 
                summarise(mean_y = mean(y)),
              aes(x = year, y = mean_y, group = factor(treated), color = factor(treated)),
              size = 1.25) +
    geom_vline(xintercept = 2004.5, linetype = "dashed", size = 0.25) +
    scale_color_manual(values = c(grey_dark, azure)) +
    scale_y_continuous(breaks = seq(-4,8,2))
    
  g10

#### sim 500
tic()
simdata01 <- map_dfr(1:500, .f = dosims, fun = sim01)
toc()

  g11 = ggplot(data = simdata01, aes(x = estimate)) +
    geom_density(fill = carolina_blue, alpha = 0.2) +
    geom_vline(xintercept = 2, linetype = "dashed", size = 0.25) +
    theme_ipsum() +
    labs(title = "Simulation 01",
         subtitle = expression(paste("Single treatment period + homogenous treatment effect (", tau, " = 2)")),
         caption = "Treatment turns on in 2005",
         x = "Estimate",
         color = "Treatment") +
    scale_x_continuous(breaks = seq(1.95,2.05,0.05), limits = c(1.95,2.05))
  
  g11
  
## SIM 02 ######################################################################



sim02 = function(...) {
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

#### sim one

data02 = sim02()

  ## plot

  g20 = ggplot(data = data02, aes(x = year, y = y, group = unit)) + 
    geom_line(alpha = 0.1, size = 0.25, color = grey_mid) +
    theme_ipsum() +
    labs(title = "Simulation 02",
         subtitle = expression(paste("Staggered treatment periods + homogenous treatment effect (", tau, " = 2)")),
         caption = "Treatment is staggered between 2000, 2008, 2016. All units are treated after 2016.",
         x = "Year",
         y = "Y",
         color = "Treatment") +
    geom_line(data = data02 %>% 
                group_by(group, year) %>% 
                summarise(mean_y = mean(y)),
              aes(x = year, y = mean_y, group = factor(group), color = factor(group)),
              size = 1.25) +
    geom_vline(xintercept = c(1999.5, 2007.5, 2015.5), linetype = "dashed", size = 0.25) +
    scale_color_manual(values = c(cool_black, azure, blue_grey)) +
    scale_y_continuous(breaks = seq(-4,8,2))
  
  g20


#### sim 500
tic()
simdata02 <- map_dfr(1:500, .f = dosims, fun = sim02)
toc()
  
  g21 = ggplot(data = simdata02, aes(x = estimate)) +
    geom_density(fill = carolina_blue, alpha = 0.2) +
    geom_vline(xintercept = 2, linetype = "dashed", size = 0.25) +
    theme_ipsum() +
    labs(title = "Simulation 02",
         subtitle = expression(paste("Staggered treatment periods + homogenous treatment effect (", tau, " = 2)")),
         caption = "Treatment is staggered between 2000, 2008, 2016. All units are treated after 2016.",
         x = "Estimate",
         color = "Treatment") +
    scale_x_continuous(breaks = seq(1.95,2.05,0.05), limits = c(1.95,2.05))
  
  g21

## SIM 03 ######################################################################

sim03 = function(...) {
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

#### sim one

data03 = sim03()

  ## plot

  g30 = ggplot(data = data03, aes(x = year, y = y, group = unit)) + 
    geom_line(alpha = 0.1, size = 0.25, color = grey_mid) +
    theme_ipsum() +
    labs(title = "Simulation 03",
         subtitle = expression(paste("Staggered treatment periods + constant treatment effect that change over time (", tau, " = 5, 3, 1)")),
         caption = "Treatment is staggered between 2000, 2008, 2016. All units are treated after 2016.",
         x = "Year",
         y = "Y",
         color = "Treatment") +
    geom_line(data = data03 %>% 
                group_by(group, year) %>% 
                summarise(mean_y = mean(y)),
              aes(x = year, y = mean_y, group = factor(group), color = factor(group)),
              size = 1.25) +
    geom_vline(xintercept = c(1999.5, 2007.5, 2015.5), linetype = "dashed", size = 0.25) +
    scale_color_manual(values = c(cool_black, azure, columbia_blue)) +
    scale_y_continuous(breaks = seq(-4,8,2))
  
  g30

#### sim 500
tic()
simdata03 <- map_dfr(1:500, .f = dosims, fun = sim03)
toc()

te = sum(c(17/50, 17/50, 16/50)*(c(5, 3, 1)))


  g21 = ggplot(data = simdata03, aes(x = estimate)) +
    geom_density(fill = carolina_blue, alpha = 0.2) +
    geom_vline(xintercept = te, linetype = "dashed", size = 0.25) +
    theme_ipsum() +
    labs(title = "Simulation 03",
         subtitle = expression(paste("Staggered treatment periods + constant treatment effect that change over time (", tau, " = 3.04)")),
         caption = "Treatment is staggered between 2000, 2008, 2016. All units are treated after 2016.",
         x = "Estimate",
         color = "Treatment") +
    scale_x_continuous(breaks = seq(2.9,3.15,0.05), limits = c(2.9,3.15))
  
  
  g21


dynamic = function(...) {
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




sim05 = function(...) {
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
    # average yearly treatment effect (by group) [\beta = 2]
    beta_hat = case_when(
      group == 2000 ~ .5,
      group == 2008 ~ .3,
      group == 2016 ~ .1
    )) %>%
    rowwise() %>% 
    mutate(beta = rnorm(1, beta_hat, 0.2), 0) %>% 
    ungroup()
  
  ## generate year specific variation
  year <- tibble(
    year = 1995:2020,
    year_fe = rnorm(26, 0, 1)
  )
  
  ## slap them together
  dat_iter = crossing(unit, year) %>% 
    mutate(e = rnorm(nrow(.), 0, 0.5),
           treat = ifelse(year >= group, 1, 0),
           tau = ifelse(treat == 1, beta, 0)) %>% 
    ## Generate time varying dependent variable
    group_by(unit) %>% 
    mutate(cumtau = cumsum(tau)) %>% 
    mutate(y = unit_fe + year_fe + cumtau + e)
  
  return(dat_iter)
}

data05 = sim05()



# Plot 3 - GB1 ---------------------------------------------------------
# Goodman-bacon decomp, overall information
data <- tibble(
  time = 0:100,
  U = seq(5, 12, length.out = 101),
  l = seq(10, 17, length.out = 101) + c(rep(0, 85), rep(15, 16)),
  k = seq(18, 25, length.out = 101) + c(rep(0, 34), rep(10, 67))
) %>% 
  pivot_longer(-time, names_to = "series", values_to = "value")

# plot
data %>% 
  ggplot(aes(x = time, y = value, group = series, color = series, shape = series)) + 
  geom_line(size = 2) + geom_point(size = 2) +
  geom_vline(xintercept = c(34, 85)) +
  labs(x = "Time", y = "Y") +
  scale_x_continuous(limits = c(0, 100), breaks = c(34, 85), 
                     expand = c(0, 0)) + 
  scale_y_continuous(limits = c(0, 40), expand = c(0, 0)) +
  scale_color_manual(values = c("#A7473A", "#4B5F6C", "#51806a")) + 
  theme_ipsum()
