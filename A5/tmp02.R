library(pacman)
p_load(tidyverse, ggplot2)

n_ind = 10
n_period = 10
n_obs = n_period*n_ind


dd_iter = function(iter = 1, n_ind = 10, n_period = 10) {
  n_obs = n_period*n_ind
  dat_iter <- tibble(
    iter = iter,
    id = rep(1:n_ind, len = n_obs),
    time = rep(1:n_period, each = n_obs / n_period),
    post = ifelse(time >= event, 1, 0),
    treated = rep(0:1, length = n_obs),
    treat = treated*post,
    group = ifelse(treated == 1 & time >= event, 1,
                   ifelse(treated == 1 & time < event, 2,
                          ifelse(treated == 0 & time >= event, 3, 4))
    ),
    y = 2 + 2*(treated == 1) + (0.2*time) + 1.5*(treat == 1) + rnorm(n_obs),
    z = 2 + 2*(treated == 1) + (0.2*time) + 1.5*(treat == 1)*(time / event) + rnorm(n_obs)
  )
    return(dat_iter)
}
    
sim_list <- map(1:500, dd_iter)
sim_df <- bind_rows(sim_list) %>% mutate(iter_group = iter + (group / 100))

ggplot(data = sim_df, aes(x = time, y = y, colour = factor(treated))) +
  geom_point(alpha = 0.1) +
  geom_vline(xintercept = 5, linetype = "dashed") +
  stat_smooth(geom='line', aes(group = iter_group), method = "lm", alpha=0.1, se=FALSE, color = "grey50") +
  geom_smooth(aes(group = group), method = "lm", se = F, color = "black")




geom_smooth(aes(group = iter_group), method = "lm", alpha = 0.2, se = F, color = "grey50")

  
  
  
  # y = 2 + 2*(control == "Treatment") + 1*(time =="After") + 1.5*(control == "Treatment")*(time=="After")+rnorm(300),
  # cont_time = (time == "Before") + 2*(time == "After") + (runif(300)-.5)*.95,
  # dgroup = ifelse(control == "Treatment" & time == "After", "post_treat",
  #                   ifelse(control == "Treatment" & time == "Before", "pre_treat",
  #                     ifelse(control == "Control" & time == "After", "post_control", "pre_control")))
  #        )

ggplot(data = dat, aes(x = cont_time, y = y, colour = control)) +
  geom_point() +
  geom_vline(xintercept = 1.5, linetype = "dashed") +
  geom_smooth(aes(group = dgroup), method = "lm", se = F)



