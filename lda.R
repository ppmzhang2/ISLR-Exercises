# =============================================================================
# Linear Discriminant Analysis (LDA) Implementation
# =============================================================================
import::from(magrittr, "%>%", "%$%", .into = "operators")

# normalize a vector
normalize <- function(x) x / sqrt(sum(x^2))

# data preparation
data(iris)
X <- iris[, 1:4] # Feature matrix
y <- iris[, 5] # Target vector
N <- 150 # Number of samples
N_K <- 50 # Number of samples per class
prior <- 1 / 3 # Prior probability

# -----------------------------------------------------------------------------
# LDA model
# -----------------------------------------------------------------------------
mdl_lda <- MASS::lda(Species ~ ., data = iris)
print(mdl_lda)

eg_vct_mdl_1 <- normalize(mdl_lda$scaling[, 1])
eg_vct_mdl_2 <- normalize(mdl_lda$scaling[, 2])

# -----------------------------------------------------------------------------
# dimentionality reduction
# -----------------------------------------------------------------------------
# entire sample
mat_x <- data.matrix(X)
mu <- colMeans(mat_x)

# matrix of each class
mat_x_1 <- data.matrix(X[1:50, ])
mat_x_2 <- data.matrix(X[51:100, ])
mat_x_3 <- data.matrix(X[101:150, ])

# mean vector of each clas
mu_1 <- colMeans(mat_x_1)
mu_2 <- colMeans(mat_x_2)
mu_3 <- colMeans(mat_x_3)

# centered matrix
cmat_x_1 <- mat_x_1 - t(replicate(50, mu_1))
cmat_x_2 <- mat_x_2 - t(replicate(50, mu_2))
cmat_x_3 <- mat_x_3 - t(replicate(50, mu_3))

# within-class scatter matrix
S_W_1 <- t(cmat_x_1) %*% cmat_x_1
S_W_2 <- t(cmat_x_2) %*% cmat_x_2
S_W_3 <- t(cmat_x_3) %*% cmat_x_3
S_W <- S_W_1 + S_W_2 + S_W_3

# between-class scatter matrix
S_B <- N_K * ((mu_1 - mu) %*% t(mu_1 - mu) +
  (mu_2 - mu) %*% t(mu_2 - mu) +
  (mu_3 - mu) %*% t(mu_3 - mu))

# eigen decomposition
ev <- eigen(solve(S_W) %*% S_B)
eg_vct_1 <- normalize(ev$vectors[, 1])
eg_vct_2 <- normalize(ev$vectors[, 2])
eg_val_1 <- ev$values[1]
eg_val_2 <- ev$values[2]


# projection
cat("1st vct ratio: ", eg_vct_mdl_1 / eg_vct_1, "\n")
cat("2nd vct ratio: ", eg_vct_mdl_2 / eg_vct_2, "\n")
cat("variance explained:", eg_val_1 / (eg_val_1 + eg_val_2), "\n")

# -----------------------------------------------------------------------------
# Classification
#
# the decision boundary is defined by a x + b = 0, where:
# a_i = sigma_inv %*% mu_i
# b_i = log(prior) - 0.5 * t(mu_i) %*% sigma_inv %*% mu_i
# -----------------------------------------------------------------------------
# shared covariance matrix
sigma <- 1 / (N - 3) * S_W
sigma_inv <- solve(sigma)

# slope and intercept
a1 <- sigma_inv %*% mu_1
a2 <- sigma_inv %*% mu_2
a3 <- sigma_inv %*% mu_3
b1 <- (log(prior) - 0.5 * t(mu_1) %*% sigma_inv %*% mu_1)[1]
b2 <- (log(prior) - 0.5 * t(mu_2) %*% sigma_inv %*% mu_2)[1]
b3 <- (log(prior) - 0.5 * t(mu_3) %*% sigma_inv %*% mu_3)[1]

# boundary
bound_12 <- mat_x_1 %*% (a1 - a2) + (b1 - b2)
bound_21 <- mat_x_2 %*% (a1 - a2) + (b1 - b2)
cat("error 1-2:", (bound_12 < 0) %>% sum(), (bound_21 > 0) %>% sum(), "\n")

bound_13 <- mat_x_1 %*% (a1 - a3) + (b1 - b3)
bound_31 <- mat_x_3 %*% (a1 - a3) + (b1 - b3)
cat("error 1-3:", (bound_13 < 0) %>% sum(), (bound_31 > 0) %>% sum(), "\n")

bound_23 <- mat_x_2 %*% (a2 - a3) + (b2 - b3)
bound_32 <- mat_x_3 %*% (a2 - a3) + (b2 - b3)
cat("error 2-3:", (bound_23 < 0) %>% sum(), (bound_32 > 0) %>% sum(), "\n")

# -----------------------------------------------------------------------------
# model performance
# -----------------------------------------------------------------------------
prd_lda <- predict(mdl_lda, X)
print(table(prd_lda$class, y))
