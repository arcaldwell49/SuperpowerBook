
design_result <- ANOVA_design(design = "4w",
                              n = 25,
                              sd = c(0.4,0.5,0.6,0.7),
                              mu = c(-0.25,0,0.10,0.15),
                              r = c(.50,.30,.15, .5,.30,.50),
                              plot = TRUE)
max(design_result$meansplot$data$mu)

design_result <- ANOVA_design(
  design = "2w*3w",
  n = 40,
  mu = c(0.3, 0, 0.5, 0.3, 0, 0),
  sd = 2,
  r = 0.8, 
  labelnames = c("condition", "cheerful", "sad", "voice", "human", "robot", "cartoon"),
  plot = FALSE
)

monte_result <- ANOVA_power(
  design_result,
  alpha_level = 0.05,
  verbose = FALSE,
  emm = TRUE,
  contrast_type = "pairwise",
  emm_p_adjust = "holm",
  nsims = 10000, #set total number of simulations
  seed = 27042020, #set seed for reproducibility
)

saveRDS(monte_result, file = "emmeans_monte1")
