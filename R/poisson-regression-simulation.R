
# Poisson simulation

generator <- function(conditions) {
  n_persons <- conditions[["n_persons"]]
  log_mean <- conditions[["log_mean"]]
  
  x <- rnorm(n_persons, 0, 1)
  b <- rnorm(1, 0, 1)
  lambda <- exp(log_mean + b*x)
  y <- rpois(n_persons, lambda)
  
  x_new <- rnorm(n_persons, 0, 1)
  lambda_new <- exp(log_mean + b*x)
  y_new <- rpois(n_persons, lambda_new)
  
  data <- list(
    x=x, 
    y=y, 
    x_new=x_new, 
    y_new=y_new,
    b=b
  )
  
  return(data)
}

simulator <- function(conditions) {
  data <- generator(conditions)
  
  n_persons <- conditions[["n_persons"]]
  log_mean <- conditions[["log_mean"]]
  
  newdata <- data[c("y_new", "x_new")]
  names(newdata) <- c("y", "x")
  
  fit_pois <- glm(y ~ x, data=data, family="poisson")
  ypred_pois <- predict(fit_pois, newdata, type="response")
  mse_pois <- mean((newdata[["y"]] - ypred_pois)^2)
  sum_pois <- summary(fit_pois)$coefficients[2,]
  
  fit_norm <- lm(y ~ x, data=data)
  ypred_norm <- predict(fit_norm, newdata)
  mse_norm <- mean((newdata[["y"]] - ypred_norm)^2)
  sum_norm <- summary(fit_norm)$coefficients[2,]
  
  output <- data.frame(
    type=c("poisson", "normal"),
    n_persons=n_persons,
    log_mean=log_mean,
    b=data$b,
    mse=c(mse_pois,mse_norm),
    rbind(sum_pois, sum_norm)
  )
  
  names(output) <- c("type", "n_persons", "log_mean","b",  "mse", "est", "se", "z", "p")
  
  return(output)
}

simulation_conditions <- expand.grid(
  n_persons=c(20, 50, 100),
  log_mean = log(c(1/10, 1, 10))
)

n_conditions <- nrow(simulation_conditions)

n_iters <- 1000

results <- data.frame()

for (c in 1:n_conditions) {
  conditions <- simulation_conditions[c,]
  
  for (i in 1:n_iters) {
    new_result <- simulator(conditions)
    results <- rbind(results, new_result)  
  
    if (i %% 100 == 0) {
      cat(
        "Sample Size: ", conditions$n_persons, 
        ", Mean: ", exp(conditions$log_mean), 
        "; Iteration: ", i, "\r"
      )
    }
  }
}

results_summary <- results %>%
  group_by(n_persons, type, log_mean) %>%
  mutate(log_mean = exp(log_mean)) %>%
  summarize(
    pred_mse = median(mse),
    bias=mean(est-b),
    mse=mean((est-b)^2),
    ll=quantile(est-b, .05),
    ul=quantile(est-b, .95),
    power = mean(p < .05, na.rm=T)
  )
pos <- position_dodge(.3)
results_summary %>%
  ggplot(aes(x = factor(n_persons), y = bias, color=type)) +
  geom_linerange(aes(ymin=bias-mse, ymax=bias+mse), linewidth=1.5, position=pos) +
  geom_point(size=3, position=pos) +
  theme_bw()

results_summary %>%
  ggplot(aes(x = factor(n_persons), y = bias, color=type)) +
  geom_linerange(aes(ymin=ll, ymax=ul), linewidth=1.5, position=pos) +
  geom_point(size=3, position=pos) +
  facet_wrap(~ log_mean) +
  theme_bw()

results_summary %>%
  ggplot(aes(x = factor(n_persons), y = power, color=type)) +
  geom_point(size=3, position=pos) +
  facet_wrap(~ log_mean) +
  theme_bw()

results_summary %>%
  ggplot(aes(x = factor(n_persons), y = log(pred_mse), color=type)) +
  geom_point(size=3, position=pos) +
  facet_wrap(~ log_mean) +
  theme_bw()


