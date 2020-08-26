# Variations Simulations

nsims = 10000
alpha_level = 0.05
library(Superpower)
library(pwr)
library(pwr2ppl)

#7.1
string <- "2b"
n <- 50
#All means are equal - so there is no real difference.
mu <- c(80, 86) 
sd <- 10
labelnames <- c("Condition", "control", "intensive_training") 
design_result <- ANOVA_design(design = string,
                              n = n, 
                              mu = mu, 
                              sd = sd, 
                              labelnames = labelnames)


simulation_result_7.1 <- ANOVA_power(design_result, 
                                 alpha_level = alpha_level, 
                                 nsims = nsims,
                                 verbose = FALSE)

save.image("data/variation_dta.RData")