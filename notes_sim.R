library(pacman)
# Packages
p_load(purrr, furrr, dplyr, tidyverse)
# Set options
set.seed(12345)
# Run 10,000 iterations
sim_list <- map(1:10, n = 1000, fun_iter)
sim_df <- bind_rows(sim_list)


fun_iter <- function(iter, n = 30) {
  # Generate data
  iter_df <- tibble(
    e = rnorm(n, sd = 15),
    x = runif(n, min = 0, max = 10),
    y = 1 + exp(0.5 * x) + e
  )
  # Estimate models
  lm1 <- lm_robust(y ~ x, data = iter_df, se_type = "classical")
  lm2 <- lm_robust(y ~ x, data = iter_df, se_type = "HC2")
  # Stack and return results
  bind_rows(tidy(lm1), tidy(lm2)) %>%
    select(1:5) %>% filter(term == "x") %>%
    mutate(se_type = c("classical", "HC2"), i = iter)
}




