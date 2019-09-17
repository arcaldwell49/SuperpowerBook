nsims <- 10000 
library(mvtnorm)
library(MASS)
library(afex)
library(emmeans)
library(gridExtra)
library(reshape2)
library(pwr)
library(pwr2ppl)
library(lsr)
library(viridis)
library(broom)
library(car)
library(tidyverse)
library(Superpower)
library(knitr)
library(kableExtra)


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


# define the parameters
# true effects (in this case, a double dissociation)
mu = c(700, 670, 670, 700) 
sigma = 150  # population standard deviation
rho = 0.75 # correlation between repeated measures
nsubs = 25 # how many subjects?
nsims = nsims # how many simulation replicates?

# create 2 factors representing the 2 independent variables
cond = data.frame(X1 = rep(factor(letters[1:2]), nsubs * 2),
                  X2 = rep(factor(letters[1:2]), nsubs, each = 2))

# create a subjects factor
subject = factor(sort(rep(1:nsubs, 4)))

# combine above into the design matrix
dm = data.frame(subject, cond)



# create k x k matrix populated with sigma
sigma.mat <- rep(sigma, 4)
S <-
  matrix(sigma.mat,
         ncol = length(sigma.mat),
         nrow = length(sigma.mat))

# compute covariance between measures
Sigma <- t(S) * S * rho  

# put the variances on the diagonal 
diag(Sigma) <- sigma^2  





# stack 'nsims' individual data frames into one large data frame
df = dm[rep(seq_len(nrow(dm)), nsims), ]

# add an index column to track the simulation run
df$simID = sort(rep(seq_len(nsims), nrow(dm)))

# sample the observed data from a multivariate normal distribution
# using MASS::mvrnorm with the mu and Sigma created earlier
# and bind to the existing df


make.y = expression(as.vector(t(mvrnorm(nsubs, mu, Sigma))))
df$y = as.vector(replicate(nsims, eval(make.y)))             

# use do(), the general purpose complement to the specialized data 
# manipulation functions available in dplyr, to run the ANOVA on
# each section of the grouped data frame created by group_by


mods <- df %>%
  group_by(simID) %>%
  do(model = aov(y ~ X1 * X2 + Error(subject / (X1 * X2)), 
                 qr = FALSE, data = .)) 

# extract p-values for each effect and store in a data frame
p_val_1 = data.frame(
  mods %>% do(as.data.frame(tidy(.$model[[3]])$p.value[1])),
  mods %>% do(as.data.frame(tidy(.$model[[4]])$p.value[1])),
  mods %>% do(as.data.frame(tidy(.$model[[5]])$p.value[1])))
colnames(p_val_1) = c('X1','X2','Interaction')

### Examine variation of means and correlation


# define the parameters
# true effects (in this case, a double dissociation)
mu = c(700, 670, 690, 750) 
sigma = 150  # population standard deviation
rho = 0.4 # correlation between repeated measures
nsubs = 25 # how many subjects?
nsims = nsims # how many simulation replicates?

# create 2 factors representing the 2 independent variables
cond = data.frame(X1 = rep(factor(letters[1:2]), nsubs * 2),
                  X2 = rep(factor(letters[1:2]), nsubs, each = 2))

# create a subjects factor
subject = factor(sort(rep(1:nsubs, 4)))

# combine above into the design matrix
dm = data.frame(subject, cond)

# create k x k matrix populated with sigma
sigma.mat <- rep(sigma, 4)
S <-
  matrix(sigma.mat,
         ncol = length(sigma.mat),
         nrow = length(sigma.mat))

# compute covariance between measures
Sigma <- t(S) * S * rho

# put the variances on the diagonal
diag(Sigma) <- sigma ^ 2  

# stack 'nsims' individual data frames into one large data frame
df = dm[rep(seq_len(nrow(dm)), nsims), ]

# add an index column to track the simulation run
df$simID = sort(rep(seq_len(nsims), nrow(dm)))

# sample the observed data from a multivariate normal distribution
# using MASS::mvrnorm with the mu and Sigma created earlier
# and bind to the existing df


make.y = expression(as.vector(t(mvrnorm(nsubs, mu, Sigma))))
df$y = as.vector(replicate(nsims, eval(make.y)))             

# use do(), the general purpose complement to the specialized data 
# manipulation functions available in dplyr, to run the ANOVA on
# each section of the grouped data frame created by group_by


mods <- df %>%
  group_by(simID) %>%
  do(model = aov(y ~ X1 * X2 + Error(subject / (X1 * X2)), 
                 qr = FALSE, data = .))

# extract p-values for each effect and store in a data frame
p_val_2 = data.frame(mods %>% 
                       do(as.data.frame(tidy(.$model[[3]])$p.value[1])),
                     mods %>% do(as.data.frame(tidy(.$model[[4]])$p.value[1])),
                     mods %>% do(as.data.frame(tidy(.$model[[5]])$p.value[1])))
colnames(p_val_2) = c('X1', 'X2', 'Interaction')

rm(subject,sigma.mat, make.y,labelnames,mu,df,mods,S,Sigma,design_result,cond,dm)

simulation_result_3.9$plot1 <- NULL
simulation_result_3.9$plot2 <- NULL
simulation_result_3.9$sim_data <- NULL

simulation_result_3.7$plot1 <- NULL
simulation_result_3.7$plot2 <- NULL
simulation_result_3.7$sim_data <- NULL

simulation_result_3.5$plot1 <- NULL
simulation_result_3.5$plot2 <- NULL
simulation_result_3.5$sim_data <- NULL