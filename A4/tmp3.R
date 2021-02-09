# Packages
library(pacman)
p_load(fixest, mvtnorm, purrr, furrr, viridis)
# Set options
set.seed(123)


cluster_iter = function(iter = 1, n_obs = 1000, n_cluster = 20) {
  rho = runif(n = 1, min = 0, max = 1)
  # individual level
  Sigma_i = matrix(c(1, 0, 0, 1 - rho), ncol = 2)
  values_i = rmvnorm(n = n_obs, sigma = Sigma_i)
  # cluster level
  cluster_name = rep(1:n_cluster, each = n_obs / n_cluster)
  Sigma_cl = matrix(c(1, 0, 0, rho), ncol = 2)
  values_cl = rmvnorm(n = n_cluster, sigma = Sigma_cl)
  d_iter = tibble(
    cluster = cluster_name,
    # error consists of individual- and cluster-level components
    e1 = values_i[ , 1] + rep(values_cl[ , 1], each = n_obs / n_cluster),
    e2 = values_i[ , 2] + rep(values_cl[ , 2], each = n_obs / n_cluster),
    # now introduce treatment that varies across i within c and across c
    treat = values_i[,1] + rep(values_cl[ , 2], each = n_obs / n_cluster),
    # data generating process
    y = 1 + treat + e1 + e2 
  )
  m = feols(data = d_iter, y ~ treat)
  m1 = m %>% tidy()
  m2 = m %>% tidy(cluster = c("cluster"))
  # m1 <- lm_robust(y ~ treat, data = d_iter) %>% tidy()
  # m2 <- lm_robust(y ~ treat, data = d_iter, clusters = cluster) %>% tidy()
  # save model estimates
  ret = bind_rows(m1, m2) %>%
    select(1:5) %>% filter(term == "treat") %>%
    mutate(diff = estimate[1] - estimate[2],
           lower = -1.96*(std.error/sqrt(n_obs)),
           upper = 1.96*(std.error/sqrt(n_obs)),
           se_type = c("normal", "clustered"),
           i = iter,
           rho = rho)
    return(ret)
}

sim_list <- map(1:500, cluster_iter)
sim_df <- bind_rows(sim_list)

g0 = ggplot(data = sim_df %>% filter(se_type == "normal")) +
  geom_point(aes(x = rho, y = diff), shape = 4, size = 0.5) +
  geom_point(aes(x = rho, y = upper), color = "") +
  geom_point(aes(x = rho, y = lower)) +
  geom_smooth(aes(x = rho, y = upper), se = FALSE, color = "black") +
  geom_smooth(aes(x = rho, y = lower), se = FALSE, color = "black")

g0 +
  geom_point(data = sim_df %>% filter(se_type == "clustered"), aes(x = rho, y = diff)) +
  geom_point(data = sim_df %>% filter(se_type == "clustered"), aes(x = rho, y = upper)) +
  geom_point(data = sim_df %>% filter(se_type == "clustered"), aes(x = rho, y = lower)) +
  geom_smooth(data = sim_df %>% filter(se_type == "clustered"), aes(x = rho, y = upper), se = FALSE, color = "red") +
  geom_smooth(data = sim_df %>% filter(se_type == "clustered"), aes(x = rho, y = lower), se = FALSE, color = "red")

  
ggplot() +
  geom_point(data = sim_df %>% filter(se_type == "normal"), aes(x = rho, y = diff)) +
  geom_point(data = sim_df %>% filter(se_type == "clustered"), aes(x = rho, y = diff),
             shape = 4, size = 2, colour = "red", alpha = 0.5) +
  geom_line(data = sim_df %>% filter == "normal", aes(x = rho, y = upper), color = "black") +
  geom_line(data = sim_df %>% filter == "normal", aes(x = rho, y = lower), color = "black") +
  geom_line(data = sim_df %>% filter == "clustered", aes(x = rho, y = upper), color = "red") +
  geom_line(data = sim_df %>% filter == "clustered", aes(x = rho, y = lower), color = "red")
  
  
  # geom_errorbar(data = sim_df %>% filter(se_type == "normal"), aes(x = rho, y = diff, ymin=lower, ymax=upper),
  #               width=.01) +
  # geom_errorbar(data = sim_df %>% filter(se_type == "clustered"), aes(x = rho, y = diff, ymin=lower, ymax=upper),
  #               width=.01, color = "red")
  
  
  geom_density(color = NA) +
  geom_hline(yintercept = 0) +
  xlab("Standard error") +
  ylab("Density") +
  scale_fill_viridis(
    "", labels = c("Classical", "Het. Robust"), discrete = T,
    option = "B", begin = 0.25, end = 0.85, alpha = 0.9) #+
  # theme_pander(base_size = 20, base_family = "Fira Sans Book")
