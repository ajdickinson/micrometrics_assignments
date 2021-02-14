library(pacman)
p_load(tidyverse, ggplot2)

p_num = 10
n_obs = 100

dat <- tibble(
  time = rep(1:p_num,
    c(rep("Before",75),rep("After",75)),2),
  control = c(rep("Control",150),rep("Treatment",150)),
  y = 2 + 2*(control == "Treatment") + 1*(time =="After") + 1.5*(control == "Treatment")*(time=="After")+rnorm(300),
  cont_time = (time == "Before") + 2*(time == "After") + (runif(300)-.5)*.95,
  dgroup = ifelse(control == "Treatment" & time == "After", "post_treat",
                    ifelse(control == "Treatment" & time == "Before", "pre_treat",
                      ifelse(control == "Control" & time == "After", "post_control", "pre_control")))
         )

ggplot(data = dat, aes(x = cont_time, y = y, colour = control)) +
  geom_point() +
  geom_vline(xintercept = 1.5, linetype = "dashed") +
  geom_smooth(aes(group = dgroup), method = "lm", se = F)

