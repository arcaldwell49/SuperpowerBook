# Mixed Multivariate
library(Superpower)

n1 = 50
n2 = 50

p = 3
S_mat <- matrix(c(16,6.0,1.6,
                  6.0,9.0,0.9,
                  1.6,0.9,1.0), nrow = 3)
W1 <- (S_mat * (n2 + n1 - 2)) / 2

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
                              sd = c(4,3,1,4,3,1),
                              r = c(rho_mat2),
                              mu = c(0,0,0,
                                     2,1.5,.2),
                              plot = TRUE)

design_result$sigmatrix

exact_result <- ANOVA_exact(design_result,
                            verbose=FALSE)

exact_result$manova_results
##########################################################
##########################################################
S_mat <- matrix(c(7.22,0.32,
                  0.32,6.992), nrow = 2)
S_mat_inverse <- solve(S_mat)
row_1 = matrix(c(-2.60,2.17), nrow = 1)
row_2 = matrix(c(-2.60,2.17), nrow = 2)

D_2 <- row_1 %*% S_mat_inverse %*% row_2

n1 = 15
n2 = 15

p = 2

pt(sqrt((n1*n2/(n1 + n2))*D_2) - 
     sqrt(((p*(n1 + n2 - 2))/(n1 + n2 - 1 - p)) *
            qf(1 - .05, p, n1 + n2 - 1 - p)), n1 + n2 - 2)

D_mat = as.vector(sqrt(diag(S_mat)))
rho_mat = (S_mat / D_mat) / D_mat[col(S_mat)]
btw_rho = rho_mat*0
rho_mat2 <- rbind(cbind(rho_mat,btw_rho),
                  cbind(btw_rho,rho_mat))

design_result <- ANOVA_design("2b*2w",
                              n = 50,
                              sd = c(sqrt(7.22),sqrt(6.992),
                                     sqrt(7.22),sqrt(6.992)),
                              r = c(rho_mat2),
                              mu = c(0,0,
                                     2.60,2.17),
                              plot = TRUE)

design_result$sigmatrix

exact_result <- ANOVA_exact(design_result,
                            verbose=FALSE)

exact_result$manova_result

aov_obj <- aov(y ~ a*b+Error(subject/b),
    data = exact_result$dataframe)

vcov(aov_obj, complete = FALSE)

