
set.seed(080125)

# one-sample t-test power analysis

generator <- function(n_persons) {
  d <- 0.20 # standardized effect size (Cohen's d)
  mu <- 0 # null hypothesis (H0) population mean
  s <- 1 # assumed standard deviation of x
  mu_x <- (d - mu) / s # mean of x given alternative hypothesis assumptions
  
  x <- rnorm(n_persons, mu_x, s)
  
  data <- data.frame(x = x)
  
  return(data)
}

simulator <- function(conditions) {
  n_persons <- conditions[["n_persons"]]
  data <- generator(n_persons)
  
  fit <- t.test(x = data$x, mu=0)
  p_val <- fit$p.value
  
  output <- data.frame(n_persons=n_persons, p_val=p_val)
  
  return(output)
}

calculate_power <- function(p_values, alpha=0.05) {
  mean(p_values < alpha)
}


simulation_conditions <- data.frame(
  n_persons = 5:250
)

n_iters <- 1000

# create an empty data.frame to store simulated p-values
results <- data.frame() 
n_conditions <- nrow(simulation_conditions)

# loop over conditions
for (c in 1:n_conditions) {
  conditions <- simulation_conditions[c,,drop=FALSE]
  # for some number of iterations
  for (i in 1:n_iters) {
    new_result <- simulator(conditions)
    
    results <- rbind(results, new_result)
    
    # print progress every 100 iterations
    if (i %% 100 == 0) {
      cat("Sample Size: ", conditions$n_persons, "; Iteration: ", i, "\r")
    }
  }
}


# Save Results (so you don't lose them)
filename <- "results/one-sample-t-test-power-analysis.csv"
write.csv(results, filename, row.names=FALSE)

# Summarize Results

simulation_summary <- aggregate(
  x = p_val ~ n_persons, 
  data=results, 
  FUN=calculate_power
)

# Plots

filename <- "fig/one-sample-t-test-simulation-results.png"
png(filename, width=2000, height=1800, res=300)
with(
  data=simulation_summary, 
  expr=plot(
    x=n_persons, y=p_val, 
    ylab="Power", xlab="Sample Size", 
    cex=1, lwd=1.5
  )
)
dev.off()

# plot with analytical solution

filename <- "fig/one-sample-t-test-simulation-results-analytic.png"
png(filename, width=2000, height=1800, res=300)
with(
  data=simulation_summary, 
  expr= {
    plot(
      x=n_persons, y=p_val, 
      ylab="Power", xlab="Sample Size", 
      cex=1, lwd=1.5
    )
    n <- 1:500
    pwr <- power.t.test(n = n, delta=0.2, sd=1, type="one.sample")$power
    min_n <- which(pwr > .8)[1]
    lines(n, pwr, lwd=3, col="blue")
    points(min_n, pwr[min_n], pch=16, col="blue", cex=2)
  }
)
dev.off()
