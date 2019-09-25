# Mixed Multivariate

S_mat <- matrix(c(16,6.0,1.6,
                  6.0,9.0,0.9,
                  1.6,0.9,1.0), nrow=3)
S_mat_inverse <- solve(S_mat)
row_1 = matrix(c(2,1.5,.2), nrow=1)
row_2 = matrix(c(2,1.5,.2), nrow=3)

D_2 <- row_1 %*% S_mat_inverse %*% row_2


?pt
