library(dplyr)
library(tidyr)
library(ggplot2)
library(viridis)
library(plotrix)
library(patchwork)

# ---- PARAMETERS ----
years <- 100
n_sims <- 10000
initial_grass <- 1  # ha
annual_N_input_per_ha <- 0.05219  # Mg N ha^-1 yr^-1

run_sim <- function(year_mean = -1.75 / 100, 
                         year_sd = 15.78 / 100 / 2, 
                         top_frac = 1.75 / 100, 
                         n_sims = 10000,
                         mean_stock,
                         sd_stock) {
  
  # ---- INITIAL STOCKS ----
  initial_values <- rnorm(n_sims, mean = mean_stock, sd = sd_stock)
  initial_values[initial_values < 0] <- 0
  
  # Replace zeros efficiently (vectorized)
  n_zeros <- sum(initial_values == 0)
  while (n_zeros > 0) {
    new_vals <- rnorm(n_zeros, mean = mean_stock, sd = sd_stock)
    new_vals[new_vals < 0] <- 0
    initial_values[initial_values == 0] <- new_vals
    n_zeros <- sum(initial_values == 0)
  }
  
  initial_stock_total <- initial_values * initial_grass
  per_ha_top_loss <- initial_values * top_frac
  
  # ---- PRE-GENERATE RANDOM RATES ----
  rate_mat <- matrix(rnorm(years * n_sims, mean = year_mean, sd = year_sd), 
                     nrow = years, ncol = n_sims)
  
  # ---- Initialize outputs ----
  remaining_area <- matrix(NA_real_, nrow = years, ncol = n_sims)
  cumulative_N   <- matrix(0, nrow = years, ncol = n_sims)
  
  # ---- VECTORIZED SIMULATION ----
  area <- rep(initial_grass, n_sims)
  total_N <- initial_stock_total
  
  for (y in 1:years) {
    area <- pmax(area * (1 + rate_mat[y, ]), 0)
    area_lost <- pmax(initial_grass - area, 0)
    
    total_N <- total_N - (area_lost * per_ha_top_loss) + (area * annual_N_input_per_ha)
    
  
    
    remaining_area[y, ] <- area
    cumulative_N[y, ] <- total_N
  }
  
  relative_remaining <- sweep(cumulative_N, 2, initial_stock_total, FUN = "/") * 100
  relative_remaining[!is.finite(relative_remaining)] <- NA_real_
  
  tibble(
    Year = rep(seq_len(years), n_sims),
    Sim = rep(seq_len(n_sims), each = years),
    Rel_N_remaining = as.vector(relative_remaining)
  )
}

# ---- MULTI-SEED WORKFLOW ----
seeds <- 1:10

# Assume long_dat exists; compute mean & sd of initial stock
mean_stock <- 7.53201
sd_stock   <- 2.936267

df_all_seeds <- lapply(seeds, function(s) {
  set.seed(s)
  
  df_list <- list(
    Zero_1   = run_sim(year_mean = 0, top_frac = 1/100, n_sims = n_sims, mean_stock = mean_stock, sd_stock = sd_stock),
    Zero_5   = run_sim(year_mean = 0, top_frac = 5/100, n_sims = n_sims, mean_stock = mean_stock, sd_stock = sd_stock),
    Zero_10   = run_sim(year_mean = 0, top_frac = 10/100, n_sims = n_sims, mean_stock = mean_stock, sd_stock = sd_stock),
    Gain_1 = run_sim(year_mean = 1/100, top_frac = 1/100, n_sims = n_sims, mean_stock = mean_stock, sd_stock = sd_stock),
    Gain_5   = run_sim(year_mean = 1/100, top_frac = 5/100, n_sims = n_sims, mean_stock = mean_stock, sd_stock = sd_stock),
    Gain_10   = run_sim(year_mean = 1/100, top_frac = 10/100, n_sims = n_sims, mean_stock = mean_stock, sd_stock = sd_stock),
    Loss_1 = run_sim(year_mean = -1/100, top_frac = 1/100, n_sims = n_sims, mean_stock = mean_stock, sd_stock = sd_stock),
    Loss_5   = run_sim(year_mean = -1/100, top_frac = 5/100, n_sims = n_sims, mean_stock = mean_stock, sd_stock = sd_stock),
    Loss_10   = run_sim(year_mean = -1/100, top_frac = 10/100, n_sims = n_sims, mean_stock = mean_stock, sd_stock = sd_stock))
  
  bind_rows(df_list, .id = "Scenario") %>%
    mutate(Seed = s)
})

df_all_seeds <- bind_rows(df_all_seeds)
df_all_seeds$Rel_N_remaining<-ifelse(df_all_seeds$Rel_N_remaining <= 0,0,df_all_seeds$Rel_N_remaining)


# ---- COMPUTE SUMMARY (MEAN + SE) ----
summary_df <- df_all_seeds %>%
  group_by(Year, Scenario) %>%
  summarise(
    mean = mean(Rel_N_remaining, na.rm = TRUE),
    sde = std.error(Rel_N_remaining, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  separate(Scenario, into = c("Scenario", "sed_loss"), sep = "_", convert = TRUE)

summary_df$sed_loss<-factor(summary_df$sed_loss)


# ---- PLOT ----
npg_colors <- pal_npg("nrc")(10)  # 2 colors for N and P
npg_colors<-npg_colors[c(2,8)]
npg_colors<-c(npg_colors,"#000000")
names(npg_colors) <- c("Gain", "Loss","Zero")

a<-ggplot(summary_df, aes(x = Year, y = mean, color = Scenario, lty = sed_loss,fill = Scenario)) +
  geom_line(size = 1.2) +
  geom_ribbon(aes(ymin = mean - (sde*1.96), ymax = mean + (sde*1.96)), alpha = 0.25) +
  ylim(c(50,150)) +
  labs(
    x = "Year",
    y = "% N Remaining",
    lty = "Sed. Lost (cm)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 12),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 12),
    plot.title = element_text(size = 12)
  ) +
  scale_color_manual(values = npg_colors) +
  scale_fill_manual(values = npg_colors, guide = "none") +
  scale_x_continuous(
    breaks = c(1, 25, 50, 75, 100),
    labels = c(2025, 2050, 2075, 2100, 2125)
  ) +
  guides(
    color = guide_legend(override.aes = list(fill = NA)),
    lty   = guide_legend(override.aes = list(fill = NA))
  ) + geom_hline(yintercept = 100, linetype = "dashed", color = "black") +
  theme(legend.position="bottom", 
        legend.justification='left',
        legend.direction='horizontal')+labs(tag = "A")



df_all_seeds <- lapply(seeds, function(s) {
  set.seed(s)
  
  df_list <- list(
    Loss_1 = run_sim(year_mean = -1/100, top_frac = 1/100, n_sims = n_sims, mean_stock = mean_stock, sd_stock = sd_stock),
    Loss_2 = run_sim(year_mean = -2.5/100, top_frac = 1/100, n_sims = n_sims, mean_stock = mean_stock, sd_stock = sd_stock),
    Loss_5 = run_sim(year_mean = -5/100, top_frac = 1/100, n_sims = n_sims, mean_stock = mean_stock, sd_stock = sd_stock),
    Loss_7 = run_sim(year_mean = -7.5/100, top_frac = 1/100, n_sims = n_sims, mean_stock = mean_stock, sd_stock = sd_stock),
    Loss_10   = run_sim(year_mean = -10/100, top_frac = 1/100, n_sims = n_sims, mean_stock = mean_stock, sd_stock = sd_stock))
  
  bind_rows(df_list, .id = "Scenario") %>%
    mutate(Seed = s)
})


df_all_seeds <- bind_rows(df_all_seeds)
df_all_seeds$Rel_N_remaining<-ifelse(df_all_seeds$Rel_N_remaining <= 0,0,df_all_seeds$Rel_N_remaining)


# ---- COMPUTE SUMMARY (MEAN + SE) ----
summary_df2 <- df_all_seeds %>%
  group_by(Year, Scenario) %>%
  summarise(
    mean = mean(Rel_N_remaining, na.rm = TRUE),
    sde = std.error(Rel_N_remaining, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  separate(Scenario, into = c("Scenario", "sed_loss"), sep = "_", convert = TRUE)

summary_df2$sed_loss<-factor(summary_df2$sed_loss)


# ---- PLOT ----
npg_colors <- pal_npg("nrc")(10)  # 2 colors for N and P
npg_colors<-npg_colors[c(2,8)]
npg_colors<-c(npg_colors,"#000000")
names(npg_colors) <- c("Gain", "Loss","Zero")

b<-ggplot(summary_df2, aes(x = Year, y = mean, lty = sed_loss)) +
  geom_line(size = 1.2,color = npg_colors[2]) +
  geom_ribbon(aes(ymin = mean - (sde*1.96), ymax = mean + (sde*1.96)), alpha = 0.25, fill = npg_colors[2]) +
  ylim(c(50,150)) +
  labs(
    x = "Year",
    y = "% N Remaining",
    lty = "Loss Rate (%)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 12),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 12),
    plot.title = element_text(size = 12)
  ) +
  scale_color_manual(values = npg_colors) +
  scale_fill_manual(values = npg_colors, guide = "none") +
  scale_x_continuous(
    breaks = c(1, 25, 50, 75, 100),
    labels = c(2025, 2050, 2075, 2100, 2125)
  ) +
  guides(
    color = guide_legend(override.aes = list(fill = NA)),
    lty   = guide_legend(override.aes = list(fill = NA))
  ) + geom_hline(yintercept = 100, linetype = "dashed", color = "black")+
  theme(legend.position="bottom", 
        legend.justification='right',
        legend.direction='horizontal')+ylab("")+
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+labs(tag = "B")

a|b
ggsave("Products//Nitrogen//simulation.png",width = 14,height = 8.5, units = "in")
