nsims = 10000
library(Superpower)
library(pwr)
library(pwr2ppl)

#rmANOVA chapter 3

K <- 2
n <- 34
sd <- 1
r <- 0.5
alpha = 0.05
f <- 0.25
f2 <- f^2
ES <- f2/(f2 + 1)
ES
mu <- mu_from_ES(K = K, ES = ES)
design = paste(K,"w",sep = "")
labelnames <- c("speed", "fast", "slow")

design_result <- ANOVA_design(design = design,
                              n = n, 
                              mu = mu, 
                              sd = sd, 
                              r = r, 
                              labelnames = labelnames)

alpha_level <- 0.05

simulation_result_3.1 <- ANOVA_power(design_result, 
                                 alpha_level = alpha_level, 
                                 nsims = nsims,
                                 verbose = FALSE)

#3.3
K <- 2
n <- 21
sd <- 1
r <- 0.7
alpha = 0.05
f <- 0.25
f2 <- f^2
ES <- f2/(f2 + 1)

mu <- mu_from_ES(K = K, ES = ES)
design = paste(K,"w",sep = "")
labelnames <- c("speed", "fast", "slow")
design_result <- ANOVA_design(design = design,
                              n = n, 
                              mu = mu, 
                              sd = sd, 
                              r = r, 
                              labelnames = labelnames)

alpha_level <- 0.05


simulation_result_3.3 <- ANOVA_power(design_result, 
                                 alpha_level = alpha_level, 
                                 nsims = nsims,
                                 verbose = FALSE)

#3.5
K <- 3
n <- 20
sd <- 1
r <- 0.8
alpha = 0.05
f <- 0.25
f2 <- f^2
ES <- f2 / (f2 + 1)

mu <- mu_from_ES(K = K, ES = ES)


sqrt(sum((mu - mean(mu)) ^ 2) / length(mu)) / sd 

design = paste(K, "w", sep = "")
labelnames <- c("speed", "fast", "medium", "slow")
design_result <- ANOVA_design(design = design,
                              n = n, 
                              mu = mu, 
                              sd = sd, 
                              r = r, 
                              labelnames = labelnames)

alpha_level <- 0.05

simulation_result_3.5 <- ANOVA_power(design_result, 
                                 alpha_level = alpha_level, 
                                 nsims = nsims,
                                 verbose = FALSE)
#3.7
design <- "3w"
n <- 75
mu <- c(0, 0.4, 0.4)
sd <- 1
r <- 0.5
labelnames <- c("speed", "fast", "medium", "slow")

design_result <- ANOVA_design(design = design,
                              n = n, 
                              mu = mu, 
                              sd = sd, 
                              r = r, 
                              labelnames = labelnames)

simulation_result_3.7 <- ANOVA_power(design_result, 
                                 alpha_level = alpha_level, 
                                 nsims = nsims,
                                 verbose = FALSE)

#3.9

design <- "3w"
n <- 75
mu <- c(0, 0.4, 0.4)
sd <- 1
r <- 0.6
labelnames <- c("SPEED", 
                "fast", "medium", "slow")

design_result <- ANOVA_design(design = design,
                              n = n, 
                              mu = mu, 
                              sd = sd, 
                              r = r, 
                              labelnames = labelnames)

simulation_result_3.9 <- ANOVA_power(design_result, 
                                 alpha_level = alpha_level, 
                                 nsims = nsims,
                                 verbose = FALSE)