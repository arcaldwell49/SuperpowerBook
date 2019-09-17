nsims = 10000
library(Superpower)
library(pwr)
library(lsr)

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

mu <- mu_from_ES(K = K, ES = ES)
mu <- mu * sd


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




#######################################################################

## Part 3

##              We first repeat the simulation by Brysbaert:
  

# Simulations to estimate the power of an ANOVA 
#with three unrelated groups
# the effect between the two extreme groups is set to d = .4, 
# the effect for the third group is d = .4 
#(see below for other situations)
# we use the built-in aov-test command
# give sample sizes (all samples sizes are equal)
N = 90
# give effect size d
d1 = .4 # difference between the extremes
d2 = .4 # third condition goes with the highest extreme
# give number of simulations
nSim = nsims
# give alpha levels
# alpha level for the omnibus ANOVA
alpha1 = .05 
#alpha level for three post hoc one-tail t-tests Bonferroni correction
alpha2 = .05 


# create vectors to store p-values
p1 <- numeric(nSim) #p-value omnibus ANOVA
p2 <- numeric(nSim) #p-value first post hoc test
p3 <- numeric(nSim) #p-value second post hoc test
p4 <- numeric(nSim) #p-value third post hoc test
pes1 <- numeric(nSim) #partial eta-squared
pes2 <- numeric(nSim) #partial eta-squared two extreme conditions

for (i in 1:nSim) {
  
  x <- rnorm(n = N, mean = 0, sd = 1)
  y <- rnorm(n = N, mean = d1, sd = 1)
  z <- rnorm(n = N, mean = d2, sd = 1)
  data = c(x, y, z)
  groups = factor(rep(letters[24:26], each = N))
  test <- aov(data ~ groups)
  pes1[i] <- etaSquared(test)[1, 2]
  p1[i] <- summary(test)[[1]][["Pr(>F)"]][[1]]
  p2[i] <- t.test(x, y)$p.value
  p3[i] <- t.test(x, z)$p.value
  p4[i] <- t.test(y, z)$p.value
  data = c(x, y)
  groups = factor(rep(letters[24:25], each = N))
  test <- aov(data ~ groups)
  pes2[i] <- etaSquared(test)[1, 2]
}

# results are as predicted when omnibus ANOVA is significant,
# t-tests are significant between x and y plus x and z; 
# not significant between y and z
# printing all unique tests (adjusted code by DL)
p1_1 = sum(p1 < alpha1) / nSim
p2_1 = sum(p2 < alpha2) / nSim
p3_1 = sum(p3 < alpha2) / nSim
p4_1 = sum(p4 < alpha2) / nSim
pes1_1 = mean(pes1)
pes2_1 = mean(pes2)

### Variation 1

# give sample sizes (all samples sizes are equal)
N = 145
# give effect size d
d1 = .4 #difference between the extremes
d2 = .0 #third condition goes with the highest extreme
# give number of simulations
nSim = nsims
# give alpha levels
#alpha level for the omnibus ANOVA
alpha1 = .05 
#alpha level for three post hoc one-tail t-test Bonferroni correction
alpha2 = .05 

# create vectors to store p-values
p1 <- numeric(nSim) #p-value omnibus ANOVA
p2 <- numeric(nSim) #p-value first post hoc test
p3 <- numeric(nSim) #p-value second post hoc test
p4 <- numeric(nSim) #p-value third post hoc test
pes1 <- numeric(nSim) #partial eta-squared
pes2 <- numeric(nSim) #partial eta-squared two extreme conditions

for (i in 1:nSim) {
  
  
  x <- rnorm(n = N, mean = 0, sd = 1)
  y <- rnorm(n = N, mean = d1, sd = 1)
  z <- rnorm(n = N, mean = d2, sd = 1)
  data = c(x, y, z)
  groups = factor(rep(letters[24:26], each = N))
  test <- aov(data ~ groups)
  pes1[i] <- etaSquared(test)[1, 2]
  p1[i] <- summary(test)[[1]][["Pr(>F)"]][[1]]
  p2[i] <- t.test(x, y)$p.value
  p3[i] <- t.test(x, z)$p.value
  p4[i] <- t.test(y, z)$p.value
  data = c(x, y)
  groups = factor(rep(letters[24:25], each = N))
  test <- aov(data ~ groups)
  pes2[i] <- etaSquared(test)[1, 2]
}

# results are as predicted when omnibus ANOVA is significant, 
# t-tests are significant between x and y plus x and z; 
# not significant between y and z
# printing all unique tests (adjusted code by DL)
p1_2 = sum(p1 < alpha1) / nSim
p2_2 = sum(p2 < alpha2) / nSim
p3_2 = sum(p3 < alpha2) / nSim
p4_2 = sum(p4 < alpha2) / nSim
pes1_2 = mean(pes1)
pes2_2 = mean(pes2)

### Variation 2


# give sample sizes (all samples sizes are equal)
N = 82
# give effect size d
d1 = .4 #difference between the extremes
d2 = .2 #third condition goes with the highest extreme
# give number of simulations
nSim = nsims
# give alpha levels
#alpha level for the omnibus ANOVA
alpha1 = .05 
#alpha level for three post hoc one-tail t-test Bonferroni correction
alpha2 = .05 

# create vectors to store p-values
p1 <- numeric(nSim) #p-value omnibus ANOVA
p2 <- numeric(nSim) #p-value first post hoc test
p3 <- numeric(nSim) #p-value second post hoc test
p4 <- numeric(nSim) #p-value third post hoc test
pes1 <- numeric(nSim) #partial eta-squared

for (i in 1:nSim) {
  #for each simulated experiment
  
  x <- rnorm(n = N, mean = 0, sd = 1)
  y <- rnorm(n = N, mean = d1, sd = 1)
  z <- rnorm(n = N, mean = d2, sd = 1)
  data = c(x, y, z)
  groups = factor(rep(letters[24:26], each = N))
  test <- aov(data ~ groups)
  pes1[i] <- etaSquared(test)[1, 2]
  p1[i] <- summary(test)[[1]][["Pr(>F)"]][[1]]
  p2[i] <- t.test(x, y)$p.value
  p3[i] <- t.test(x, z)$p.value
  p4[i] <- t.test(y, z)$p.value
  data = c(x, y)
  groups = factor(rep(letters[24:25], each = N))
  test <- aov(data ~ groups)
  pes2[i] <- etaSquared(test)[1, 2]
}

p1_3 = sum(p1 < alpha1) / nSim
p2_3 = sum(p2 < alpha2) / nSim
p3_3 = sum(p3 < alpha2) / nSim
p4_3 = sum(p4 < alpha2) / nSim
pes1_3 = mean(pes1)
pes2_3 = mean(pes2)


###Reduce data load

rm(design_result, test, x, y, z, data)

rm(groups)

simulation_result_2.19$sim_data <- NULL
simulation_result_2.19$plot1 <- NULL
simulation_result_2.19$plot2 <- NULL

simulation_result_2.21$sim_data <- NULL
simulation_result_2.21$plot1 <- NULL
simulation_result_2.21$plot2 <- NULL

simulation_result_2.17$sim_data <- NULL
simulation_result_2.17$plot1 <- NULL
simulation_result_2.17$plot2 <- NULL

simulation_result_2.11$sim_data <- NULL
simulation_result_2.11$plot1 <- NULL
simulation_result_2.11$plot2 <- NULL

simulation_result_2.13$sim_data <- NULL
simulation_result_2.13$plot1 <- NULL
simulation_result_2.13$plot2 <- NULL

simulation_result_2.9$sim_data <- NULL
simulation_result_2.9$plot1 <- NULL
simulation_result_2.9$plot2 <- NULL

simulation_result_2.7$sim_data <- NULL
simulation_result_2.7$plot1 <- NULL
simulation_result_2.7$plot2 <- NULL