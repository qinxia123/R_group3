#' Linear Regression Function
#'
#' This function performs multiple linear regression using ordinary least squares
#' and returns an object of class 'linreg' containing various statistics.
#'
#' @param formula A formula specifying the model to be fitted (e.g., y ~ x1 + x2).
#' @param data A data frame containing the variables in the model.
#' @return An object of class 'linreg' containing the following statistics:
#'   \item{coefficients}{Estimated coefficients for the regression model.}
#'   \item{fitted_values}{Fitted values from the regression.}
#'   \item{residuals}{Residuals (difference between observed and fitted values).}
#'   \item{degrees_of_freedom}{Degrees of freedom for the regression.}
#'   \item{residual_variance}{Estimated variance of the residuals.}
#'   \item{variance_coefficients}{Variance of the regression coefficients.}
#'   \item{t_values}{t-values for each coefficient.}
#'   \item{p_values}{p-values for each coefficient.}
#' @export
linreg <- function(formula, data) {
  # Create the design matrix X
  X <- model.matrix(formula, data)
  
  # Pick out the dependent variable y
  y_var <- all.vars(formula)[1]
  y <- data[[y_var]]
  
  # Calculate the regression coefficients
  beta_hat <- solve(t(X) %*% X) %*% t(X) %*% y
  
  # Calculate the fitted values
  y_hat <- X %*% beta_hat
  
  # Calculate the residuals
  e_hat <- y - y_hat
  
  # Calculate degrees of freedom
  n <- nrow(X)  # Sample size
  p <- ncol(X)  # Number of parameters
  df <- n - p   # Degrees of freedom
  
  # Calculate the residual variance
  sigma_hat_squared <- sum(e_hat^2) / df
  
  # Calculate the variance of the regression coefficients
  var_beta <- sigma_hat_squared * solve(t(X) %*% X)
  
  # Calculate the t-values for each coefficient
  t_values <- beta_hat / sqrt(diag(var_beta))
  
  # Calculate the p-values for each coefficient
  p_values <- 2 * pt(-abs(t_values), df = df)
  
  # Create a list to store results
  result <- list(
    coefficients = beta_hat,
    fitted_values = y_hat,
    residuals = e_hat,
    degrees_of_freedom = df,
    residual_variance = sigma_hat_squared,
    variance_coefficients = var_beta,
    t_values = t_values,
    p_values = p_values
  )
  
  # Set the class of the result to 'linreg'
  class(result) <- "linreg"
  
  return(result)
}