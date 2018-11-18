# Two factor CFA simulation

# Muthen, L. K., and Muthen, B. O. (2002). How to use a monte carlo study to
# decide on sample size and determin power. Structural Equation Modeling, 9,
# 599-620.

#The first criterion is that parameter and standard error biases do not exceed
#10% for any parameter in the model. The second criterion is that the standard
#error bias for the parameter for which power is being assessed does not exceed
#5%. The third criterion is that coverage remains between 0.91 and 0.98. Once
#these three conditions are satisfied, the sample size is chosen to keep power
#close to 0.80. The value of 0.80 is used because it is a commonly accepted
#value for sufficient power.


library(lavaan)

Ns <- seq(50, 400, by = 50)
itemsPerFac <- 5
loadings <- .8
facCor <- .25


model <- '
  
  F1 =~ x1 + x2 + x3 + x4 + x5
  F2 =~ y1 + y2 + y3 + y4 + y5

  F1 ~~ cor1*F2 
  
  F1 ~~ varF1*F1  
  F2 ~~ varF2*F2  

  x1 ~~ residx1*x1
  x2 ~~ residx2*x2
  x3 ~~ residx3*x3
  x4 ~~ residx4*x4
  x5 ~~ residx5*x5
  
  y1 ~~ residy1*y1
  y2 ~~ residy2*y2
  y3 ~~ residy3*y3
  y4 ~~ residy4*y4
  y5 ~~ residy5*y5
  
'

oneFacCfa.sim <- function(N = N, items = items, loadings = loadings, model = model) {
  require(lavaan)
  F1 <- rnorm(N, 0, 1)
  X <- as.data.frame(replicate(items, loadings * F1 + (sqrt(1 - loadings^2) * rnorm(N, 0, 1))))
  names(X) <- paste0("x", 1:items)
  mod = cfa(model = model, data = X, std.lv = T)
  fits = as.data.frame(t(as.matrix(fitmeasures(mod)[c("chisq", "df", "cfi", "tli", "aic", "bic", "rmsea", "srmr")])))
  pars = partable(mod)
  return(cbind(pars, fits, N, items))
}

twoFacCfa.sim <- function(N = N, itemsPerFac = itemsPerFac, loadings = loadings, model = model, facCor = facCor) {
  require(lavaan)
  F1 <- rnorm(N, 0, 1)
  X <- as.data.frame(replicate(itemsPerFac, loadings * F1 + (sqrt(1 - loadings^2) * rnorm(N, 0, 1))))
  names(X) <- paste0("x", 1:itemsPerFac)
  F2 <- facCor * F1 + rnorm(N, 0, 1)
  Y <- as.data.frame(replicate(itemsPerFac, loadings * F2 + (sqrt(1 - loadings^2) * rnorm(N, 0, 1))))
  names(Y) <- paste0("y", 1:itemsPerFac)
  mod = cfa(model = model, data = cbind(X, Y), std.lv = T)
  fits = as.data.frame(t(as.matrix(fitmeasures(mod)[c("chisq", "df", "cfi", "tli", "aic", "bic", "rmsea", "srmr")])))
  pars = parameterestimates(mod)
  return(cbind(pars, fits, N, itemsPerFac))
}
sim1 <- data.frame()
for (N in Ns) {
  sim <- replicate(10, twoFacCfa.sim(N = N, itemsPerFac = itemsPerFac, loadings = loadings, model = model, facCor = facCor), simplify = F)
  
  sim1 <- rbind(sim1, do.call(rbind.data.frame, sim))
}

sim1$coverage <- ifelse(with(sim1, (ci.lower <= est & est <= ci.upper)), 1,0)
agg <- c("est", "se", "chisq", "df", "cfi", "tli", "aic", "bic", "rmsea", "srmr", "z", "pvalue", "ci.lower", "ci.upper", "coverage")
aggby <- with(sim1, list("lhs" = lhs, "op" = op, "rhs" = rhs, "N" = N, "itemsPerFac" = itemsPerFac))
sim1.agg <- aggregate(sim1[,agg], by = aggby, mean, na.rm = T)
sim1.agg$power <- unlist(aggregate(sim1[, "pvalue"], by = aggby, function(x) mean(x < .05, na.rm = T))["x"])
sim1.agg$parSD <- unlist(aggregate(sim1[, "est"], by = aggby, function(x) sd(x, na.rm = T))["x"])
sim1.agg$reliability <- NA


sim1.agg$reliability[which(sim1.agg$lhs == "F1" & sim1.agg$op == "=~")] <- 
  (sim1.agg$est[which(sim1.agg$lhs == "F1" & sim1.agg$op == "=~")]^2 * sim1.agg$est[which(sim1.agg$lhs == "F1" & sim1.agg$rhs == "F1")])/
  ((sim1.agg$est[which(sim1.agg$lhs == "F1" & sim1.agg$op == "=~")]^2 * sim1.agg$est[which(sim1.agg$lhs == "F1" & sim1.agg$rhs == "F1")]) +
     sim1.agg$est[grep("x", sim1.agg$lhs)]) 

sim1.agg$reliability[which(sim1.agg$lhs == "F2" & sim1.agg$op == "=~")] <- 
  (sim1.agg$est[which(sim1.agg$lhs == "F2" & sim1.agg$op == "=~")]^2 * sim1.agg$est[which(sim1.agg$lhs == "F2" & sim1.agg$rhs == "F2")])/
  ((sim1.agg$est[which(sim1.agg$lhs == "F2" & sim1.agg$op == "=~")]^2 * sim1.agg$est[which(sim1.agg$lhs == "F2" & sim1.agg$rhs == "F2")]) +
     sim1.agg$est[grep("y", sim1.agg$lhs)]) 
sim1.agg$parBias <- NA
sim1.agg$parBias[which(sim1.agg$op == "=~")] <- (sim1.agg$est[which(sim1.agg$op == "=~")] - loadings)/loadings
sim1.agg$seBias <- (sim1.agg$se - sim1.agg$parSD)/sim1.agg$parSD
sim1.agg$par <- with(sim1.agg, paste0(lhs, op, rhs))


library(ggplot2)
ggplot(sim1.agg[which(sim1.agg$par == "F1~~F2"),], aes(x = N, y = power)) +
  geom_line() +
  geom_hline(yintercept = .8, linetype = 2) +
  theme_bw()
