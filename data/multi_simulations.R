design_result <- ANOVA_design("4w",
                              n = 29,
                              r = c(.05,.15,.25,.55, .65, .9
                              ),
                              sd = 1,
                              mu= c(0,0,0,0))

#In order to simulate violations we MUST use ANOVA_power
power_result_s1 <- ANOVA_power(design_result, nsims = nsims, verbose = FALSE)




design_result <- ANOVA_design("4w",
                              n = 29,
                              r = c(.05,.15,.25,.55, .65, .9
                              ),
                              sd = c(1,3,5,7),
                              mu= c(0,0,0,0))

power_result_none <- ANOVA_power(design_result, nsims = nsims, verbose = FALSE)

power_result_gg <- ANOVA_power(design_result, correction = "GG",
                               nsims = nsims, verbose = FALSE)

power_result_hf <- ANOVA_power(design_result, correction = "HF",
                               nsims = nsims, verbose = FALSE)



design_result_power <- ANOVA_design("4w",
                              n = 29,
                              r = c(.05,.15,.25,.55, .65, .9
                              ),
                              sd = c(1,3,5,7),
                              mu= c(0,0.75,1.5,3))



power_result_hfeffect <- ANOVA_power(design_result_power, correction = "HF",
                                     nsims = nsims, verbose = FALSE)




design_result_power2 <- ANOVA_design("4w",
                                    n = 50,
                                    r = c(.05,.15,.25,.55, .65, .9
                                    ),
                                    sd = c(1,3,5,7),
                                    mu= c(0,0.75,1.5,3))



power_result_hfeffect2 <- ANOVA_power(design_result_power2, correction = "HF",
                                     nsims = nsims, verbose = FALSE)

power_result_hfeffect2$sim_data <- NULL
power_result_hfeffect2$plot1 <- NULL
power_result_hfeffect2$plot2 <- NULL

power_result_hfeffect$sim_data <- NULL
power_result_hfeffect$plot1 <- NULL
power_result_hfeffect$plot2 <- NULL

power_result_none$sim_data <- NULL
power_result_none$plot1 <- NULL
power_result_none$plot2 <- NULL

power_result_s1$sim_data <- NULL
power_result_s1$plot1 <- NULL
power_result_s1$plot2 <- NULL

power_result_s2$sim_data <- NULL
power_result_s2$plot1 <- NULL
power_result_s2$plot2 <- NULL

power_result_gg$sim_data <- NULL
power_result_gg$plot1 <- NULL
power_result_gg$plot2 <- NULL
power_result_gg$pc_results <- NULL

power_result_hf$sim_data <- NULL
power_result_hf$plot1 <- NULL
power_result_hf$plot2 <- NULL
power_result_hf$pc_results <- NULL

rm(design_result_power)
rm(design_result_power2)


