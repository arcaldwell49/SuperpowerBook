nsims = 10000
library(Superpower)
library(pwr)
library(pwr2ppl)

#mixedANOVA Chapter 4

mu <- c(-0.25, 0.25, 0.25,-0.25)
n <- 23
sd <- 1
r <- 0.5
string = "2w*2b"
alpha_level <- 0.05
labelnames = c("age", "old", "young", "color", "blue", "red")

design_result <- ANOVA_design(
  design = string,
  n = n,
  mu = mu,
  sd = sd,
  r = r,
  labelnames = labelnames
)


simulation_result_4.1 <- ANOVA_power(design_result, 
                                 alpha_level = alpha_level, 
                                 nsims = nsims,
                                 verbose = FALSE)

#4.3
mu <- c(-0.25, 0.25, 0.25, -0.25)
n <- 23
sd <- 1
r <- 0.7
string = "2w*2b"
alpha_level <- 0.05
labelnames = c("age", "old", "young", "color", "blue", "red")
design_result <- ANOVA_design(design = string,
                              n = n, 
                              mu = mu, 
                              sd = sd, 
                              r = r, 
                              labelnames = labelnames)

simulation_result_4.3 <- ANOVA_power(design_result, 
                                 alpha_level = alpha_level, 
                                 nsims = nsims,
                                 verbose = FALSE)

#4.5
# true effects (in this case, a double dissociation)
mu = c(700, 670, 670, 700) 
sigma = 150  # population standard deviation
n <- 25
sd <- 150
r <- 0.75
string = "2w*2w"
alpha_level <- 0.05
labelnames = c("age", "old", "young", "color", "blue", "red")
design_result <- ANOVA_design(design = string,
                              n = n, 
                              mu = mu, 
                              sd = sd, 
                              r = r, 
                              labelnames = labelnames)

simulation_result_4.5 <- ANOVA_power(design_result, 
                                 alpha_level = alpha_level, 
                                 nsims = nsims,
                                 verbose = FALSE)

#4.7

# true effects (in this case, a double dissociation)
mu = c(700, 670, 690, 750) 
sigma = 150  # population standard deviation
n <- 25
sd <- 150
r <- 0.4
string = "2w*2w"
alpha_level <- 0.05
labelnames = c("AGE", "old", "young", 
               "COLOR", "blue", "red")

design_result <- ANOVA_design(design = string,
                              n = n, 
                              mu = mu, 
                              sd = sd, 
                              r = r, 
                              labelnames = labelnames)

simulation_result_4.7 <- ANOVA_power(design_result, 
                                 alpha_level = 0.05, 
                                 nsims = nsims,
                                 verbose = FALSE)

#4.9

mu = c(2,1,4,2) 
n <- 20
sd <- 5
r <- c(
  0.8, 0.4, 0.4,
  0.4, 0.4,
  0.8
)
string = "2w*2w"
alpha_level <- 0.05
labelnames = c("A", "a1", "a2", "B", "b1", "b2")

design_result <- ANOVA_design(design = string,
                              n = n, 
                              mu = mu, 
                              sd = sd, 
                              r = r, 
                              labelnames = labelnames)

simulation_result_4.9 <- ANOVA_power(design_result, 
                                 alpha_level = alpha_level, 
                                 nsims = nsims,
                                 verbose = FALSE)