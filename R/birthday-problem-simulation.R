

set.seed(1)

generator <- function(n_persons) {
  days_of_year <- 1:365
  bdays <- sample(days_of_year, n_persons, replace = TRUE)
  
  data <- data.frame(bdays=bdays)
  
  return(data)
}

simulator <- function(conditions) {
  n_persons <- conditions[["n_persons"]]
  
  data <- generator(n_persons)
  
  bday_tally <- with(data, table(bdays))
  result <- any(bday_tally > 1)
  
  output <- data.frame(n_persons=n_persons, any_same=result)
  
  return(output)
}


simulation_conditions <- data.frame(n_persons = 2:100)

n_conditions <- nrow(simulation_conditions)
n_iters <- 1000
results <- data.frame()

for (c in 1:n_conditions) {
  conditions <- simulation_conditions[c,,drop=FALSE]
  
  for (i in 1:n_iters) {
    result <- simulator(conditions)
    results <- rbind(results, result)
  }
    
  cat("Number of people at the party:", conditions[["n_persons"]], "\r")
    
}

filename <- "results/birthday-party-problem.csv"
write.csv(results, filename, row.names=FALSE)  

results_summary <- aggregate(any_same ~ n_persons, data=results, FUN=mean)

analytic_prob <- function(n_persons) {
  k <- 365
  
  log_vnr <- lgamma(k+1) - lgamma(k-n_persons + 1)
  log_vt <- log(k) * n_persons
  p <- exp(log_vnr - log_vt)
  
  return(p)
} 

filename <- "fig/birthday-party-simulation-results.png"
png(filename, width = 2000, height=1800, res=275)


with(
  data=results_summary,
  expr = {
    a <- 1-analytic_prob(23)
    plot(
      x=n_persons, y=any_same, 
      ylim=c(0, 1),
      ylab="Probability",
      xlab="Number of people",
      main="How likely is it that two people at a party have the same birthday?",
      type="s", lwd=3
    )
    
    n <- results_summary$n_persons[results_summary$any_same > .5][1]
    p <- results_summary$any_same[n-1]
    txt <- paste0(n, " people")
    points(n, p, pch=16, cex=2)
    text(n+4, p, txt, adj = 0)
    txt <- bquote(p %~~% .(round(a*100, 2)) ~~ "%")
    text(n+4, .5-.05, txt, adj = 0)
  }
)
dev.off()
lines(2:100, a, lty=1, lwd=3, col="blue")



