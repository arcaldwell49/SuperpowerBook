# Error Simulations
nsims = 10000
alpha_level = 0.05
library(Superpower)

design_result_violation <-ANOVA_design(
  design = "2b*2b",
  n = c(20, 80, 40, 80),
  mu = c(0, 0, 0, 0),
  sd = c(3, 1, 5, 1),
  labelnames = c("condition", "cheerful", "sad", "voice", "human", "robot"))

power_result_violation <- ANOVA_power(
   design_result_violation,
   nsims = nsims,
   verbose = TRUE)

design_result_violation_2 <-ANOVA_design(
  design = "2b*2b",
  n = c(80, 80, 80, 80),
  mu = c(0, 0, 0, 0),
  sd = c(3, 1, 5, 1),
  labelnames = c("condition", "cheerful", "sad", "voice", "human", "robot"))

power_result_violation_2 <- ANOVA_power(
   design_result_violation_2,
   nsims = nsims,
   verbose = TRUE)

# Violations of sphericity
 
design_result_violation_3 <- ANOVA_design(design = "4w",
                                          n = 29,
                                          r = c(.05,.15,.25,.55, .65, .9),
                                          sd = c(1,3,5,7),
                                          mu= c(0,0,0,0))

# In order to simulate violations we use ANOVA_power

power_result_violation_3 <- ANOVA_power(design_result_violation_3, 
                                        alpha_level = 0.05, 
                                        nsims = nsims, 
                                        verbose = FALSE)

power_result_violation_4 <- ANOVA_power(design_result_violation_3, 
                                        alpha_level = 0.05, 
                                        nsims = nsims,
                                        correction = "GG",
                                        verbose = FALSE)

power_result_violation_5 <- ANOVA_power(design_result_violation_3, 
                                        alpha_level = 0.05, 
                                        nsims = nsims,
                                        correction = "HF",
                                        verbose = FALSE)

design_result_power <- ANOVA_design("4w",
                                    n = 29,
                                    r = c(.05,.15,.25,.55, .65, .9),
                                    sd = c(1,3,5,7),
                                    mu= c(0,0.75,1.5,3))

power_result_hfeffect <- ANOVA_power(design_result_power, 
                                     correction = "HF",
                                     nsims = nsims, 
                                     verbose = FALSE)

save.image("data/assumption_violation.Rdata")