# Packages
library(pacman)
p_load(fixest, mvtnorm, purrr, furrr, viridis)
# Set options
set.seed(123)


cluster_iter = function(iter = 1, n_obs = 1000, n_cluster = 20) {
  rho = runif(n = 1, min = 0, max = 1)
  ## individual level
  Sigma_i = matrix(c(1, 0, 0, 1 - rho), ncol = 2)
  values_i = rmvnorm(n = n_obs, sigma = Sigma_i)
  ## cluster level
  cluster_name = rep(1:n_cluster, each = n_obs / n_cluster)
  Sigma_cl = matrix(c(1, 0, 0, rho), ncol = 2)
  values_cl = rmvnorm(n = n_cluster, sigma = Sigma_cl)
  d_iter = tibble(
    cluster = cluster_name,
    ## error consists of individual- and cluster-level components
    e1 = values_i[ , 1] + rep(values_cl[ , 1], each = n_obs / n_cluster),
    e2 = values_i[ , 2] + rep(values_cl[ , 2], each = n_obs / n_cluster),
    ## now introduce treatment that varies across i
    treat_i = rep(0:1, each = n_obs / 2),
    treat_c = rep(0:1, each = n_obs / n_cluster, len = n_obs),
    ## data generating process for both treatment levels i and c
    y_i = 1 + treat_i + e1 + e2,
    y_c = 1 + treat_c + e1 + e2
  )
  ## run each model; treatment at i and c
  m_i = feols(data = d_iter, y_i ~ treat_i)
  m_c = feols(data = d_iter, y_c ~ treat_c)
  ## package into single data frame that will output into a list
  m_i1 = m_i %>% tidy()
  m_i2 = m_i %>% tidy(cluster = c("cluster"))
  m_c1 = m_c %>% tidy()
  m_c2 = m_c %>% tidy(cluster = c("cluster"))
  ret = bind_rows(m_i1, m_i2, m_c1, m_c2) %>%
    select(1:5) %>% filter(term %in% c("treat_i", "treat_c")) %>%
    mutate(diff_i = estimate[1] - estimate[2],
           diff_c = estimate[3] - estimate[4],
           lower = -1.96*(std.error/sqrt(n_obs)),
           upper = 1.96*(std.error/sqrt(n_obs)),
           se_type = c("normal", "clustered","normal", "clustered"),
           i = iter,
           rho = rho)
    return(ret)
}

sim_list <- map(1:5, cluster_iter)
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
