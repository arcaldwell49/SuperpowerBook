nsims = 10000
library(Superpower)
library(pwr)


#Table 2.1
string <- "2b"
n <- 100
# We are thinking of running 50 peope in each condition
mu <- c(24, 26.2)
# Enter means in the order that matches the labels below.
# In this case, control, cat, dog. 
sd <- 6.4
labelnames <- c("condition", "control", "pet") #
# the label names should be in the order of the means specified above.
design_result <- ANOVA_design(design = string,
                              n = n, 
                              mu = mu, 
                              sd = sd, 
                              labelnames = labelnames)
alpha_level <- 0.05
# You should think carefully about how to justify your alpha level.
# We will give some examples later, but for now, use 0.05.
simulation_result_2.1 <- ANOVA_power(design_result, 
                                 alpha_level = alpha_level, 
                                 verbose = FALSE,
                                 nsims = 10000)


K <- 2
n <- 100
sd <- 6.4
r <- 0
f <- pwr.anova.test(n = n,
                    k = K,
                    power = 0.9,
                    sig.level = alpha_level)$f
f2 <- f^2
ES <- f2 / (f2 + 1)
ES
mu <- mu_from_ES(K = K, ES = ES)
mu <- mu * sd
mu

design_result <- ANOVA_design(
  design = string,
  n = n,
  mu = mu,
  sd = sd,
  labelnames = labelnames
)

simulation_result_2.3 <- ANOVA_power(design_result, 
                                 alpha_level = alpha_level, 
                                 nsims = 10000,
                                 verbose = FALSE)

string <- "2b"
n <- 179
mu <- c(24, 26.2)
# Enter means in the order that matches the labels below.
# In this case, control, pet.
sd <- 6.4
labelnames <- c("condition", "control", "pet") #
# the label names should be in the order of the means specified above.
design_result <- ANOVA_design(
  design = string,
  n = n,
  mu = mu,
  sd = sd,
  labelnames = labelnames
)
alpha_level <- 0.05

simulation_result_2.5 <- ANOVA_power(design_result, 
                                 alpha_level = alpha_level, 
                                 nsims = 10000,
                                 verbose = FALSE)

string <- "3b"
n <- 50
# We are thinking of running 50 peope in each condition
mu <- c(24, 26.2, 26.6)
# Enter means in the order that matches the labels below.
# In this case, control, cat, dog.
sd <- 6.4
labelnames <- c("condition", "control", "cat", "dog") #
# the label names should be in the order of the means specified above.
design_result <- ANOVA_design(
  design = string,
  n = n,
  mu = mu,
  sd = sd,
  labelnames = labelnames
)
alpha_level <- 0.05
# You should think carefully about how to justify your alpha level.
# We will give some examples later, but for now, use 0.05.
simulation_result_2.7 <- ANOVA_power(design_result, 
                                 alpha_level = alpha_level, 
                                 nsims = 10000,
                                 verbose = FALSE)
#2.9

K <- 3
n <- 50
sd <- 6.4
r <- 0
#Calculate f when running simulation
f <- pwr.anova.test(n = n,
                    k = K,
                    power = 0.9,
                    sig.level = alpha_level)$f

## [1] 0.2934417

f2 <- f^2
ES <- f2 / (f2 + 1)

## [1] 0.07928127

mu <- mu_from_ES(K = K, ES = ES)
mu <- mu * sd


design_result <- ANOVA_design(
  design = string,
  n = n,
  mu = mu,
  sd = sd,
  labelnames = labelnames
)
simulation_result_2.9 <- ANOVA_power(design_result,
                                 alpha_level = alpha_level,
                                 nsims = nsims,
                                 verbose = FALSE)


#2.11

K <- 3
mu <- c(0, 0.4, 0.4)
n <- 90
sd <- 1
r <- 0
design = paste(K, "b", sep = "")

design_result <- ANOVA_design(
  design = string,
  n = n,
  mu = mu,
  sd = sd,
  labelnames = c("factor1", "level1", "level2", "level3")
)
simulation_result_2.11 <- ANOVA_power(design_result,
                                 alpha_level = alpha_level,
                                 nsims = nsims,
                                 verbose = FALSE)

#2.13


K <- 3
mu <- c(0, 0.4, 0.0)
n <- 145
sd <- 1
r <- 0
design = paste(K, "b", sep = "")

design_result <- ANOVA_design(
  design = string,
  n = n,
  mu = mu,
  sd = sd,
  labelnames = c("factor1", "level1", "level2", "level3")
)

simulation_result_2.13 <- ANOVA_power(design_result, 
                                 alpha_level = alpha_level, 
                                 nsims = 10000,
                                 verbose = FALSE)

#2.15

K <- 3
mu <- c(0, 0.4, 0.2)
n <- 82
sd <- 1
design = paste(K, "b", sep = "")

design_result <- ANOVA_design(
  design = string,
  n = n,
  mu = mu,
  sd = sd,
  labelnames = c("factor1", "level1", "level2", "level3")
)

simulation_result_2.15 <- ANOVA_power(design_result, 
                                 alpha_level = alpha_level, 
                                 nsims = nsims,
                                 verbose = FALSE)

#2.17

K <- 3
ES <- .0099
mu <- mu_from_ES(K = K, ES = ES)
n <- 5000
sd <- 1
r <- 0
string = paste(K,"b",sep = "")

design_result <- ANOVA_design(
  design = string,
  n = n,
  mu = mu,
  sd = sd,
  r = r,
  labelnames = c("factor1", "level1", "level2", "level3")
)

simulation_result_2.17 <- ANOVA_power(design_result, 
                                 alpha_level = alpha_level, 
                                 nsims = nsims,
                                 verbose = FALSE)
#2.19


K <- 4
ES <- .0588
mu <- mu_from_ES(K = K, ES = ES)
n <- 5000
sd <- 1
r <- 0
string = paste(K,"b",sep = "")

design_result <- ANOVA_design(
  design = string,
  n = n,
  mu = mu,
  sd = sd,
  r = r,
  labelnames = c("factor1", "level1", "level2", "level3", "level4")
)

simulation_result_2.19 <- ANOVA_power(design_result, 
                                 alpha_level = alpha_level, 
                                 nsims = nsims,
                                 verbose = FALSE)

#2.21

K <- 2
ES <- .1379
mu <- mu_from_ES(K = K, ES = ES)
n <- 5000
sd <- 1
r <- 0
string = paste(K,"b",sep = "")

design_result <- ANOVA_design(design = string,
                              n = n, 
                              mu = mu, 
                              sd = sd, 
                              r = r, 
                              labelnames = c("factor1", "level1", "level2"))

simulation_result_2.21 <- ANOVA_power(design_result, 
                                 alpha_level = alpha_level, 
                                 nsims = nsims,
                                 verbose = FALSE)