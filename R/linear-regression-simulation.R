

# Basic regression simulation ####

generator <- function(conditions) {
  # extract meta-data from conditions
  n_persons <- conditions[["n_persons"]]
  
  # regression parameters
  a <- rnorm(1, 0, 1)
  b1 <- rnorm(1, 0, 1)
  b2 <- rnorm(1, 0, 1)
  sigma <- rexp(1, 1)
  
  # simulate predictors
  x1 <- rnorm(n_persons, 0, 1)
  x2 <- rnorm(n_persons, 0, 1)
  
  # regression equation
  mu <- a + b1*x1 + b2*x2
  
  # collect parameters in a list to be returned
  pars <- c(a=a, b1=b1, b2=b2, sigma=sigma)
  
  # sample dependent variable responses
  y <- rnorm(n_persons, mu, sigma)
  
  # create dataset with simulated data
  data <- data.frame(y=y, x1=x1, x2=x2)
  
  # collect data and parameters in a list to return as the function output
  output <- list(data=data, pars=pars)
  
  return(output)
}


estimator <- function(data) {
  
  # estimate 1PL model for 1 factor with data provided
  model_fit <- lm(y ~ x1 + x2, data=data)
  
  # extract model coefficients
  model_summary <- summary(model_fit)
  est_pars <- model_summary$coefficients
  est_pars <- rbind(est_pars, c(sigma(model_fit), NA, NA, NA))
  dimnames(est_pars) <- list(NULL, c("est", "se", "tval", "pval"))
  par_names <- c("intercept", "b1", "b2", "sigma")
  pars <- data.frame(par=par_names, est_pars)
  
  return(pars)
}

simulator <- function(generator, estimator, conditions) {
  
  # generate data
  output <- generator(conditions)
  
  # extract data from generator output
  data <- output$data
  
  # extract true parameters from generator output
  true_pars <- output$pars
  
  # estimate parameters from data using linear regression estimator
  est_pars <- estimator(data)
  
  # compile estimated and true parameters with conditions
  result <- data.frame(
    est_pars,
    "true"=unname(true_pars), 
    c(conditions)
  )
  
  return(result)
}

sample_sizes <- c(50, 100, 500)
n_iters <- 1000

simulation_conditions <- data.frame(
  n_persons=sample_sizes
)

n_conditions <- nrow(simulation_conditions)

simulation_results <- data.frame()

for (c in 1:n_conditions) {
  for (i in 1:n_iters) {
    conditions <- simulation_conditions[c,,drop=FALSE]
    result <- simulator(generator, estimator, conditions)
    result$iteration <- i
    
    simulation_results <- rbind(simulation_results, result)
    
    if (i %% 25 == 0) {
      cat(
        paste0(
          "Sample Size: ", conditions[["n_persons"]],
          ", Test Length: ", conditions[["n_items"]],
          ", Iterations: ", i, " out of ", n_iters,
          "\n"
        )
      )
      
    }
  }
}

results_filename <- "results/regression-simulation-results.csv"
write.csv(simulation_results, results_filename, na="", row.names=FALSE)


# analyze results ####

library(tidyverse)

alpha <- 0.05
crit_z <- qnorm(1-alpha/2, 0, 1)

results_summary <- simulation_results %>%
  filter(par %in% c("b1", "b2")) %>%
  mutate(
    bias = true - est,
    ll = true-crit_z*se,
    ul = true+crit_z*se
  ) %>%
  group_by(n_persons) %>%
  summarize(
    n_sim = n(),
    mean_bias = mean(bias),
    rmse = sqrt(mean(bias^2)),
    mean_se = sqrt(mean(se^2)),
    coverage = mean(ll < est & est < ul),
    power = mean(pval < alpha),
    r = cor(true, est)
  )







