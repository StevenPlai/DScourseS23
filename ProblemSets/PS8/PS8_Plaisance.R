library(tidyverse)

set.seed(100)

# Set values for N and K
N <- 100000
K <- 10

# Generate matrix X with 1st column of 1's and remaining columns of normal random numbers
X <- matrix(rnorm(N * K, mean = 0, sd = 1), nrow = N, ncol = K)
X[,1] <- 1

# Generate vector eps of normal random numbers with mean 0 and standard deviation 0.5
eps <- rnorm(N, mean = 0, sd = 0.5)

# Generate vector beta with the given values
beta <- c(1.5, -1, -0.25, 0.75, 3.5, -2, 0.5, 1, 1.25, 2)

# Generate vector Y as X%*%beta + eps
Y <- X %*% beta + eps

beta_hat <- solve(t(X) %*% X) %*% t(X) %*% Y

#Gradient Descent

learning_rate <- 0.0000003

# Define the number of iterations
num_iterations <- 10000

# Initialize beta to a vector of 0's
beta <- rep(0, K)

# Define the gradient vector function
gradient <- function(X, Y, beta) {
  2 * t(X) %*% (X %*% beta - Y) / N
}

# Perform gradient descent
for (i in 1:num_iterations) {
  beta <- beta - learning_rate * gradient(X, Y, beta)
}

# Display the OLS estimate of beta obtained using gradient descent
beta_hat_gd <- beta

# Define the objective function to be minimized
obj_fun <- function(beta) {
  sum((Y - X %*% beta) ^ 2)
}

# Set initial values of beta to a vector of 0's
beta0 <- rep(0, K)

# Set bounds for beta
lower_bounds <- rep(-Inf, K)
upper_bounds <- rep(Inf, K)
bounds <- list(lower = lower_bounds, upper = upper_bounds)

# Use L-BFGS algorithm to obtain OLS estimate of beta
library(nloptr)
sol_LBFGS <- nloptr(x0 = beta0, eval_f = obj_fun, lb = lower_bounds, ub = upper_bounds, opts = list("algorithm"="NLOPT_LN_SBPLX", "maxeval"=1000))
beta_hat_LBFGS <- sol_LBFGS$solution

# Use Nelder-Mead algorithm to obtain OLS estimate of beta
sol_NM <- nloptr(x0 = beta0, eval_f = obj_fun, lb = lower_bounds, ub = upper_bounds, opts = list("algorithm"="NLOPT_LN_NELDERMEAD", "maxeval"=1000))
beta_hat_NM <- sol_NM$solution

# Display the OLS estimate of beta obtained using L-BFGS algorithm
beta_hat_LBFGS

# Display the OLS estimate of beta obtained using Nelder-Mead algorithm
beta_hat_NM

# Define the negative log-likelihood function to be minimized
neg_loglik <- function(theta, Y, X) {
  beta <- theta[1:(length(theta)-1)]
  sig <- theta[length(theta)]
  loglik <- -sum(dnorm(Y - X %*% beta, mean = 0, sd = sig, log = TRUE))
  return(loglik)
}

# Define the gradient function
gradient <- function(theta, Y, X) {
  grad <- as.vector(rep(0, length(theta)))
  beta <- theta[1:(length(theta)-1)]
  sig <- theta[length(theta)]
  grad[1:(length(theta)-1)] <- -t(X) %*% (Y - X %*% beta) / (sig^2)
  grad[length(theta)] <- dim(X)[1] / sig - crossprod(Y - X %*% beta) / (sig^3)
  return(grad)
}

# Set initial values of theta to a vector of 1's
theta0 <- c(rep(1, K), 1)

# Set bounds for theta
lower_bounds <- c(rep(-Inf, K), 0.001)
upper_bounds <- c(rep(Inf, K), Inf)
bounds <- list(lower = lower_bounds, upper = upper_bounds)

# Use L-BFGS algorithm to obtain MLE estimate of beta
sol_LBFGS <- nloptr(x0 = theta0, eval_f = neg_loglik, eval_grad_f = gradient, lb = lower_bounds, ub = upper_bounds, opts = list("algorithm"="NLOPT_LN_SBPLX", "maxeval"=1000), Y = Y, X = X)
beta_hat_MLE <- sol_LBFGS$solution[1:K]

# Display the MLE estimate of beta obtained using L-BFGS algorithm
beta_hat_MLE

# OLS using lm()
ols_lm <- lm(Y ~ X - 1)
# Output regression results using modelsummary
library(modelsummary)
library(kableExtra)
table <- modelsummary(ols_lm)
save_kable(table,file = "summaryTable.png",zoom=1.5)

beta <- c(1.5, -1, -0.25, 0.75, 3.5, -2, 0.5, 1, 1.25, 2)

#Find errors
mean(abs(beta-beta_hat))
mean(abs(beta-beta_hat_gd))
mean(abs(beta-beta_hat_LBFGS))
mean(abs(beta-beta_hat_NM))
mean(abs(beta-coef(ols_lm)))
     