# Mixed Multivariate
library(Superpower)

n1 = 50
n2 = 50

p = 3
S_mat <- matrix(c(16,6.0,1.6,
                  6.0,9.0,0.9,
                  1.6,0.9,1.0), nrow = 3)
W1 <- (S_mat * (n2 + n1 - 2)) / 2
W1_mat = as.vector(sqrt(diag(W1)))
W1_cor = (W1 / W1_mat) / W1_mat[col(W1)]
btw_rho = W1_cor*0
rho_mat_tot <- rbind(cbind(W1_cor, btw_rho),
                     cbind(btw_rho, W1_cor))

S_mat_inverse <- solve(S_mat)
row_1 = matrix(c(2,1.5,.2), nrow = 1)
row_2 = matrix(c(2,1.5,.2), nrow = 3)

D_2 <- row_1 %*% S_mat_inverse %*% row_2

T_2 <- (n1*n2)/(n1 + n2)*D_2
F_val <- ((n1 + n2 - p - 1)/((n1 + n2 - 2)*p)) * T_2

Ft <- qf((1 - .05), 1, 98)

power <- (1 - pf(Ft,
        1,
        98,
        F_val)) * 100

pt(sqrt((n1*n2/(n1 + n2))*D_2) - 
     sqrt(((p*(n1 + n2 - 2))/(n1 + n2 - 1 - p)) *
            qf(1 - .05, p, n1 + n2 - 1 - p)), n1 + n2 - 2)

D_mat = as.vector(sqrt(diag(S_mat)))
rho_mat = (S_mat / D_mat) / D_mat[col(S_mat)]
btw_rho = rho_mat*0
rho_mat2 <- rbind(cbind(rho_mat,btw_rho),
                  cbind(btw_rho,rho_mat))

design_result <- ANOVA_design("2b*3w",
                              n = 50,
                              sd = c(28,21,7,28,21,7),
                              r = rho_mat_tot,
                              mu = c(0,0,0,
                                     2,1.5,.2),
                              plot = TRUE)

design_result$sigmatrix

exact_result <- ANOVA_exact(design_result,
                            verbose=FALSE)

exact_result$manova_results
##########################################################
##########################################################
n1 = 15
n2 = 15

p = 2

S_mat <- matrix(c(7.22,0.32,
                  0.32,6.992), nrow = 2)

W1 <- (S_mat * (n2 + n1 - 2)) / 2
W1_mat = as.vector(sqrt(diag(W1)))
W1_cor = (W1 / W1_mat) / W1_mat[col(W1)]
btw_rho = W1_cor*0
rho_mat_tot <- rbind(cbind(W1_cor, btw_rho),
                     cbind(btw_rho, W1_cor)) 
diag(rho_mat_tot) <- 1


S_mat_inverse <- solve(S_mat)
row_1 = matrix(c(-2.60,2.17), nrow = 1)
row_2 = matrix(c(-2.60,2.17), nrow = 2)

D_2 <- row_1 %*% S_mat_inverse %*% row_2



pt(sqrt((n1*n2/(n1 + n2))*D_2) - 
     sqrt(((p*(n1 + n2 - 2))/(n1 + n2 - 1 - p)) *
            qf(1 - .05, p, n1 + n2 - 1 - p)), n1 + n2 - 2)

D_mat = as.vector(sqrt(diag(S_mat)))
rho_mat = (S_mat / D_mat) / D_mat[col(S_mat)]
btw_rho = rho_mat*0
rho_mat2 <- rbind(cbind(rho_mat,btw_rho),
                  cbind(btw_rho,rho_mat))

design_result <- ANOVA_design("2b*2w",
                              n = 15,
                              sd = c(18.80904,18.50967,
                                     18.80904,18.50967),
                              r = c(rho_mat_tot),
                              mu = c(0,0,
                                     2.60,2.17),
                              plot = TRUE)

design_result$sigmatrix

exact_result <- ANOVA_exact(design_result,
                            verbose=FALSE)

exact_result$manova_result

####################
### SAS Manual ppg 3795
####################

cor_1 <- matrix(c(1,.6,.491,.399,
                  .6,1,.495,.402,
                  .491,.495,1,.491,
                  .399,.402,.491,1), nrow=4)

cor_2 <- cor_1*0

pain_cor_mat <- cbind(rbind(cor_1,cor_2),
                      rbind(cor_2,cor_1))

design_result <- ANOVA_design("2b*4w",
                              n = 476,
                              mu = c(2.4, 2.38, 2.05, 1.90,
                                     2.4, 2.39, 2.36, 2.30),
                              sd = .92,
                              r = pain_cor_mat,
                              labelnames = c("Treatment", "sensory", "standard",
                                             "TIME", "t1", "t2", "t3", "t4"),
                              plot = TRUE)
exact_result <- ANOVA_exact(design_result, verbose = FALSE,
                            alpha_level = .01)

exact_result$manova_results

design_result <- ANOVA_design("2b*4w",
                              n = 476,
                              mu = c(2.4, 2.38, 2.05, 1.90,
                                     2.4, 2.39, 2.36, 2.30),
                              sd = 1.04,
                              r = pain_cor_mat,
                              labelnames = c("Treatment", "sensory", "standard",
                                             "TIME", "t1", "t2", "t3", "t4"),
                              plot = TRUE)
exact_result <- ANOVA_exact(design_result, verbose = FALSE,
                            alpha_level = .01)

exact_result$manova_results
