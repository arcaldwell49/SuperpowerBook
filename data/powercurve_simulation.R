nsims = 10000
library(Superpower)
library(pwr)
library(tidyverse)
library(viridis)

string <- "2w*2w"
labelnames = c("A", "a1", "a2", "B", "b1", "b2")
design_result <- ANOVA_design(design = string,
                              n = 20, 
                              mu = c(0,0,0,0.0), 
                              sd = 1, 
                              r = 0.5, 
                              labelnames = labelnames)
p_a <- plot_power(design_result,
                  max_n = 100)
p_a$power_df$effect <- 0

design_result <- ANOVA_design(design = string,
                              n = 20, 
                              mu = c(0,0,0,0.1), 
                              sd = 1, 
                              r = 0.5, 
                              labelnames = labelnames)
p_b <- plot_power(design_result,
                  max_n = 100)

p_b$power_df$effect <- 0.1

design_result <- ANOVA_design(design = string,
                              n = 20, 
                              mu = c(0,0,0,0.2), 
                              sd = 1, 
                              r = 0.5, 
                              labelnames = labelnames)
p_c <- plot_power(design_result,
                  max_n = 100)

p_c$power_df$effect <- 0.2

design_result <- ANOVA_design(design = string,
                              n = 20, 
                              mu = c(0,0,0,0.3), 
                              sd = 1, 
                              r = 0.5, 
                              labelnames = labelnames)
p_d <- plot_power(design_result,
                  max_n = 100)

p_d$power_df$effect <- 0.3

design_result <- ANOVA_design(design = string,
                              n = 20, 
                              mu = c(0,0,0,0.4), 
                              sd = 1, 
                              r = 0.5, 
                              labelnames = labelnames)
p_e <- plot_power(design_result,
                  max_n = 100)

p_e$power_df$effect <- 0.4

design_result <- ANOVA_design(design = string,
                              n = 20, 
                              mu = c(0,0,0,0.5), 
                              sd = 1, 
                              r = 0.5, 
                              labelnames = labelnames)
p_f <- plot_power(design_result,
                  max_n = 100)

p_f$power_df$effect <- 0.5


plot_data <- rbind(p_a$power_df, p_b$power_df, p_c$power_df, 
                   p_d$power_df, p_e$power_df, p_f$power_df)

plot1 <- ggplot(plot_data, aes(x = n, y = `A:B`,color = as.factor(effect))) +
  geom_line(size = 1.5) +
  labs(color = "Effect Size") +
  scale_color_viridis_d()

####

design_result <- ANOVA_design(design = string,
                              n = 20, 
                              mu = c(0,0,0,0.0), 
                              sd = 1, 
                              r = 0.5, 
                              labelnames = labelnames)
p_a <- plot_power(design_result,
                  max_n = 100)
p_a$power_df$effect <- 0

design_result <- ANOVA_design(design = string,
                              n = 20, 
                              mu = c(0.1,0,0,0.1), 
                              sd = 1, 
                              r = 0.5, 
                              labelnames = labelnames)
p_b <- plot_power(design_result,
                  max_n = 100)

p_b$power_df$effect <- 0.1

design_result <- ANOVA_design(design = string,
                              n = 20, 
                              mu = c(0.2,0,0,0.2), 
                              sd = 1, 
                              r = 0.5, 
                              labelnames = labelnames)
p_c <- plot_power(design_result,
                  max_n = 100)

p_c$power_df$effect <- 0.2

design_result <- ANOVA_design(design = string,
                              n = 20, 
                              mu = c(0.3,0,0,0.3), 
                              sd = 1, 
                              r = 0.5, 
                              labelnames = labelnames)
p_d <- plot_power(design_result,
                  max_n = 100)

p_d$power_df$effect <- 0.3

design_result <- ANOVA_design(design = string,
                              n = 20, 
                              mu = c(0.4,0,0,0.4), 
                              sd = 1, 
                              r = 0.5, 
                              labelnames = labelnames)
p_e <- plot_power(design_result,
                  max_n = 100)

p_e$power_df$effect <- 0.4


design_result <- ANOVA_design(design = string,
                              n = 20, 
                              mu = c(0.5,0,0,0.5), 
                              sd = 1, 
                              r = 0.5, 
                              labelnames = labelnames)
p_f <- plot_power(design_result,
                  max_n = 100)

p_f$power_df$effect <- 0.5

plot_data <- rbind(p_a$power_df, p_b$power_df, p_c$power_df,
                   p_d$power_df, p_e$power_df, p_f$power_df)

plot2 <- ggplot(plot_data, aes(x = n, y = `A:B`,color = as.factor(effect))) +
  geom_line(size = 1.5) +
  labs(color = "Effect Size") +
  scale_color_viridis_d()

##

string <- "2w*2w"
labelnames = c("A", "a1", "a2", "B", "b1", "b2")
design_result <- ANOVA_design(design = string,
                              n = 20, 
                              mu = c(0,0,0,0.3), 
                              sd = 1, 
                              r = 0.0, 
                              labelnames = labelnames)
p_a <- plot_power(design_result,
                  max_n = 100)

p_a$power_df$correlation <- 0.0 


design_result <- ANOVA_design(design = string,
                              n = 20, 
                              mu = c(0,0,0,0.3), 
                              sd = 1, 
                              r = 0.1, 
                              labelnames = labelnames)
p_b <- plot_power(design_result,
                  max_n = 100)

p_b$power_df$correlation <- 0.1


design_result <- ANOVA_design(design = string,
                              n = 20, 
                              mu = c(0,0,0,0.3), 
                              sd = 1, 
                              r = 0.3, 
                              labelnames = labelnames)
p_c <- plot_power(design_result,
                  max_n = 100)

p_c$power_df$correlation <- 0.3


design_result <- ANOVA_design(design = string,
                              n = 20, 
                              mu = c(0,0,0,0.3), 
                              sd = 1, 
                              r = 0.5, 
                              labelnames = labelnames)
p_d <- plot_power(design_result,
                  max_n = 100)

p_d$power_df$correlation <- 0.5


design_result <- ANOVA_design(design = string,
                              n = 20, 
                              mu = c(0,0,0,0.3), 
                              sd = 1, 
                              r = 0.7, 
                              labelnames = labelnames)
p_e <- plot_power(design_result,
                  max_n = 100)

p_e$power_df$correlation <- 0.7


design_result <- ANOVA_design(design = string,
                              n = 20, 
                              mu = c(0,0,0,0.3), 
                              sd = 1, 
                              r = 0.9, 
                              labelnames = labelnames)
p_f <- plot_power(design_result,
                  max_n = 100)

p_f$power_df$correlation <- 0.9

plot_data <- rbind(p_a$power_df, p_b$power_df, 
                   p_c$power_df, p_d$power_df, p_e$power_df, p_f$power_df)

plot3 <- ggplot(plot_data, aes(x = n, y = `A:B`, color = as.factor(correlation))) +
  geom_line(size = 1.5) +
  labs(color = "Correlation") +
  scale_color_viridis_d()
##############

string <- "2w*2w"
labelnames = c("A", "a1", "a2", "B", "b1", "b2")
design_result <- ANOVA_design(design = string,
                              n = 20, 
                              mu = c(0,0,0,0.3), 
                              sd = 1, 
                              r <- c(
                                0.4, 0.4, 0.4,
                                0.4, 0.4,
                                0.4),
                              labelnames = labelnames)
p_a <- plot_power(design_result,
                  max_n = 100)

p_a$power_df$corr_diff <- 0

design_result <- ANOVA_design(design = string,
                              n = 20, 
                              mu = c(0,0,0,0.3), 
                              sd = 1, 
                              r <- c(
                                0.5, 0.4, 0.4,
                                0.4, 0.4,
                                0.5),
                              labelnames = labelnames)
p_b <- plot_power(design_result,
                  max_n = 100)

p_b$power_df$corr_diff <- 0.1


design_result <- ANOVA_design(design = string,
                              n = 20, 
                              mu = c(0,0,0,0.3), 
                              sd = 1, 
                              r <- c(
                                0.6, 0.4, 0.4,
                                0.4, 0.4,
                                0.6),
                              labelnames = labelnames)
p_c <- plot_power(design_result,
                  max_n = 100)

p_c$power_df$corr_diff <- 0.2

design_result <- ANOVA_design(design = string,
                              n = 20, 
                              mu = c(0,0,0,0.3), 
                              sd = 1, 
                              r <- c(
                                0.7, 0.4, 0.4,
                                0.4, 0.4,
                                0.7), 
                              labelnames = labelnames)
p_d <- plot_power(design_result,
                  max_n = 100)

p_d$power_df$corr_diff <- 0.3

design_result <- ANOVA_design(design = string,
                              n = 20, 
                              mu = c(0,0,0,0.3), 
                              sd = 1, 
                              r <- c(
                                0.8, 0.4, 0.4,
                                0.4, 0.4,
                                0.8), 
                              labelnames = labelnames)
p_e <- plot_power(design_result,
                  max_n = 100)

p_e$power_df$corr_diff <- 0.4

design_result <- ANOVA_design(design = string,
                              n = 20, 
                              mu = c(0,0,0,0.3), 
                              sd = 1, 
                              r <- c(
                                0.9, 0.4, 0.4,
                                0.4, 0.4,
                                0.9), 
                              labelnames = labelnames)
p_f <- plot_power(design_result,
                  max_n = 100)

p_f$power_df$corr_diff <- 0.5

plot_data <- rbind(p_a$power_df, p_b$power_df, p_c$power_df, 
                   p_d$power_df, p_e$power_df, p_f$power_df)


plot4 <- ggplot(plot_data, aes(x = n, y = `A`, color = as.factor(corr_diff))) +
  geom_line(size = 1.5) +
  labs(color = "Difference in Correlation") +
  scale_color_viridis_d()
