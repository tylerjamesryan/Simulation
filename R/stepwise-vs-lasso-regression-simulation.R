
library(tidyverse)

rlaplace <- function(n, tau) {
  lambda <- 1 / tau
  x1 <- rexp(n, lambda)
  x2 <- rexp(n, lambda)
  
  return(x1 - x2)
}


sample_pars <- function(conditions) {
  n_persons <- conditions[["n_persons"]]
  n_pred <- conditions[["n_pred"]]
  
  a <- rnorm(1, 0, 1)
  tau <- rexp(1, 10) # amount of regularization, higher numbers = fewer predictors
  b <- rlaplace(n_pred, tau)
  sigma <- rexp(1, 1)
  
  pars <- list(a=a, tau=tau, b=b, sigma=sigma)
  
  return(pars)
}

generator <- function(conditions, pars) {
  n_persons <- conditions[["n_persons"]]
  n_pred <- conditions[["n_pred"]]
  
  with(
    data=pars,
    expr = {
      X <- matrix(rnorm(n_persons*n_pred, 0, 1), n_persons, n_pred)
      colnames(X) <- paste0("x", 1:n_pred)
      
      mu <- a + X%*%b
      y <- rnorm(n_persons, mu, sigma)
      
      data <- list(y=y, X=X)
      
      data
    }
  )
}
  


estimator_stepwise <- function(data) {
  y <- data$y
  X <- data$X
  
  data <- data.frame(y=y, X)
  
  fit <- lm(y ~ ., data=data)
  fit <- step(fit, trace=0)
  
  return(fit)
}

estimator_lasso <- function(data) {
  require(glmnet)
  
  y <- data$y
  X <- data$X
  
  fit <- cv.glmnet(X, y, nfolds=5, )
  
  return(fit)
}

loss_fn <- function(data, fit) {
  
  if (class(fit) == "lm") {
    df <- with(data, data.frame(y, X))
    yhat <- predict(fit, newdata=df)
    mse <- mean((data$y - yhat)^2)
  } else {
    mse <- with(data, assess.glmnet(fit, X, y))$mse[[1]]
  }
  
  return(mse)
}

simulator <- function(conditions) {
  n_persons <- conditions[["n_persons"]]
  n_pred <- conditions[["n_pred"]]
  
  if (n_persons <= n_pred) {
    return(NULL)
  }
  proceed <- FALSE
  
  while(!proceed) {
    pars <- sample_pars(conditions)
    data <- generator(conditions, pars)
    
    fit_stepwise <- tryCatch(estimator_stepwise(data))
    fit_lasso <- tryCatch(estimator_lasso(data))
    
    if (class(fit_stepwise) == "try-error" | class(fit_lasso) == "try-error") {
      next
    } else {
      proceed <- TRUE
    }
  }
  
  data <- generator(conditions, pars)
  
  mse_stepwise <- loss_fn(data, fit_stepwise)
  mse_lasso <- loss_fn(data, fit_lasso)
  
  result <- data.frame(
    type=c("stepwise", "lasso"),
    n_persons = conditions[["n_persons"]],
    n_pred = conditions[["n_pred"]],
    mse=c(mse_stepwise, mse_lasso)
    #k=c(length(fit_stepwise$model), length(b_lasso))
  )
  
  return(result)
}


simulation_conditions <- expand.grid(
  n_persons=c(50, 250, 500), n_pred=c(5, 20, 50, 100)
)

n_cond <- nrow(simulation_conditions)
n_iters <- 500

results <- data.frame()

for (c in 1:n_cond) {
  conditions <- simulation_conditions[c,,drop=FALSE]
  result <- replicate(n_iters, simulator(conditions), simplify=FALSE) %>%
    bind_rows(.id="iter")
  
  results <- rbind(results, result)
  
  print(conditions)
}

filename <- "results/stepwise-vs-lasso-regression-results.csv"
write.csv(results, filename, row.names=FALSE)


results %>%
  group_by(type, n_persons, n_pred) %>%
  summarize(m = sqrt(mean(mse))) %>%
  ggplot(aes(x = n_persons, y = m, color=type)) +
  geom_line() +
  facet_wrap(~ n_pred) +
  theme_bw()
