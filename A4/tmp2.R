# Packages
library(pacman)
p_load(fixest, mvtnorm, purrr, furrr)
# Set options
set.seed(123)


gen_cluster <- function(n = 1000, n_cluster = 20, rho = 1) {
  # individual level 
  Sigma_i <- matrix(c(1, 0, 0, 1 - rho), ncol = 2)
  values_i <- rmvnorm(n = n, sigma = Sigma_i)
  # cluster level
  cluster_name <- rep(1:n_cluster, each = n / n_cluster)
  Sigma_cl <- matrix(c(1, 0, 0, rho), ncol = 2)
  values_cl <- rmvnorm(n = n_cluster, sigma = Sigma_cl)
  # error consists of individual- and cluster-level components
  e1 <- values_i[ , 1] + rep(values_cl[ , 1], each = n / n_cluster)
  e2 <- values_i[ , 2] + rep(values_cl[ , 2], each = n / n_cluster)
  # now introduce treatment that varies across i within c and across c
  treat = values_i[,1] + rep(values_cl[ , 2], each = n / n_cluster)
  # data generating process
  y <- 1 + treat + e1 + e2 
  d_iter <- tibble(cluster = cluster_name, e1, e2, y, treat)
  dim(d_iter) %>% return()
  # regress y on treat
  m = feols(data = d_iter, y ~ treat)
  m1 = m %>% tidy()
  m2 = m %>% tidy(cluster = c("cluster"))
  # m1 = lm_robust(y ~ treat, data = d_iter, clusters = cluster) %>% tidy()
  # m2 = lm_robust(y ~ treat, data = d_iter, clusters = cluster) %>% tidy()
  # save model estimates
  bind_rows(m1, m2) %>%
    select(1:5) %>% filter(term == "treat") %>%
    mutate(se_type = c("normal", "clustered"), i = n) %>% 
    return()
}





flex_iter <- function(iter, γ = 0, δ = 1, n = 30) {
  # Generate data
  iter_df <- tibble(
    x = runif(n, min = 0, max = 10),
    ε = rnorm(n, sd = 15 * x^δ),
    y = 1 + exp(γ * x) + ε
  )
  # Estimate models
  lm1 <- lm_robust(y ~ x, data = iter_df, se_type = "classical")
  lm2 <- lm_robust(y ~ x, data = iter_df, se_type = "HC2")
  # Stack and return results
  bind_rows(tidy(lm1), tidy(lm2)) %>%
    select(1:5) %>% filter(term == "x") %>%
    mutate(se_type = c("classical", "HC2"), i = iter)
}

sim_test = map(1:2, flex_iter)
