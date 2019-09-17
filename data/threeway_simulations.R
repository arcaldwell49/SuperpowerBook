# Three Way Simulations

nsims = 10000
alpha_level = 0.05
library(Superpower)
library(pwr)
library(pwr2ppl)

#Chapter 5 NoWayInteraction

# With 2x2x2 designs, 
# the names for paired comparisons can become very long. 
# So here I abbreviate terms: 
#   Size, Color, and Cognitive Load, have values:
# b = big, s = small, g = green, 
# r = red, pres = present, abs = absent.  
labelnames <- c("Size", "b", "s", "Color", "g", "r", 
                "Load", "pres", "abs") #
design_result <- ANOVA_design(design = "2b*2b*2b", 
                              #sample size per group 
                              n = 50, 
                              #pattern of means
                              mu = c(2, 2, 6, 1, 6, 6, 1, 8), 
                              sd = 10, #standard deviation
                              labelnames = labelnames) 

simulation_result_5.1 <- ANOVA_power(design_result, 
                                 alpha_level = alpha_level, 
                                 nsims = nsims,
                                 verbose = FALSE)

#5.3
string <- "2b"
n <- 200
mu <- c(mean(c(2, 2, 6, 1)), mean(c(6, 6, 1, 8)))
sd <- 10
labelnames <- c("Size", "big", "small")
design_result <- ANOVA_design(design = string,
                              n = n, 
                              mu = mu, 
                              sd = sd, 
                              labelnames = labelnames)

simulation_result_5.3 <- ANOVA_power(design_result, 
                                 alpha_level = alpha_level, 
                                 nsims = nsims,
                                 verbose = FALSE)

#5.5

string <- "2b*2b"
n <- 100
mu <- c(mean(c(1, 1)), mean(c(6, 1)), mean(c(6, 6)), mean(c(1, 6)))
sd <- 10
labelnames <- c("Size", "big", "small", "Color", "green", "red")
design_result <- ANOVA_design(design = string,
                              n = n, 
                              mu = mu, 
                              sd = sd, 
                              labelnames = labelnames)

simulation_result_5.5 <- ANOVA_power(design_result, 
                                 alpha_level = alpha_level, 
                                 nsims = nsims,
                                 verbose = FALSE)

#5.7
string <- "2b*2b*2b"
n <- 50
mu <- c(5, 3, 2, 6, 1, 4, 3, 1) 
sd <- 10
r <- 0.0
labelnames <- c("Size", "big", "small", 
                "Color", "green", "red", 
                "CognitiveLoad", "present", "absent") 
design_result <- ANOVA_design(design = string,
                              n = n, 
                              mu = mu, 
                              sd = sd, 
                              labelnames = labelnames)

simulation_result_5.7 <- ANOVA_power(design_result, 
                                 alpha_level = alpha_level, 
                                 nsims = nsims,
                                 verbose = FALSE)