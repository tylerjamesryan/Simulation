
library(mirt)
library(tidyverse)

# Basic item response theory simulation ####

# create 1PL item response function
# P(y=1 | theta, beta) = 1 / (1 + exp(-(theta-beta)))
item_response_function <- function(theta, beta) {
  logodds <- theta - beta # calculate response log-odds
  prob <- 1 / (1 + exp(-logodds)) # inverse logit transform to get probability
  
  return(prob)
}

generator <- function(conditions) {
  
  n_persons <- conditions[["n_persons"]]
  n_items <- conditions[["n_items"]]
  
  # population parameters (aka hyperparameters)
  ability_mean <- 0 # population mean
  ability_sd <- 1 # population standard deviation
  
  # person-specific latent ability scores (theta)
  latent_ability <- rnorm(n_persons, ability_mean, ability_sd)
  
  # item-specific difficulty parameters (beta)
  item_difficulty <- rnorm(n_items, 0, 1)
  
  # collect parameters in a list to be returned
  pars <- list(
    latent_ability=latent_ability,
    item_difficulty=item_difficulty
  )
  
  # create matrix to put responses in
  data <- matrix(data=NA, nrow=n_persons, ncol=n_items)
  dimnames(data) <- list(
    person=paste0("p", 1:n_persons),
    item=paste0("i", 1:n_items)
  )
  
  # Loop over persons and items to generate responses
  
  for (p in 1:n_persons) { # for each person, p
    for (i in 1:n_items) { # responding to each item, i
      
      # calculate the probability of a correct response, P(y=1 | theta, beta)
      prob <- item_response_function(latent_ability[p], item_difficulty[i])
      
      # draw a response from a binomial distribution with size=1
      y <- rbinom(n=1, size=1, prob=prob)
      
      # record the response in the data matrix
      data[p,i] <- y
    }
  }
  
  # collect data and parameters in a list to return as the function output
  output <- list(data=data, pars=pars)
  
  return(output)
}


extract_item_pars <- function(fit) {
  item_pars <- coef(fit, IRTpars=TRUE, printSE=TRUE)
  
  i_names <- names(item_pars)
  i_names <- i_names[grep("i\\d", i_names)]
  
  item_pars <- t(sapply(item_pars[i_names], function(item) item[,"b"]))
  item_pars <- as.data.frame(item_pars)
  colnames(item_pars) <- c("est", "SE")
  
  return(item_pars)
}

extract_person_pars <- function(fit) {
  person_pars <- fscores(fit, full.scores.SE = TRUE)
  colnames(person_pars) <- c("est", "SE")
  
  return(person_pars)
}

estimator <- function(data) {
  require(mirt) # load mirt if not already loaded
  
  # estimate 1PL model for 1 factor with data provided
  model_fit <- mirt(
    data=data, 
    model=1, # 1-factor cfa
    itemtype="1PL",
    SE = TRUE, # we want parameter standard errors
    verbose = FALSE # FALSE so it doesn't flood the console with messages
  )
  
  # extract estimated person and item parameters
  latent_ability <- extract_person_pars(model_fit)
  
  item_difficulty <- extract_item_pars(model_fit)
  
  pars <- list(latent_ability=latent_ability, item_difficulty=item_difficulty)
  
  return(pars)
}

check_num_of_unique_responses <- function(x) {
  length(unique(x)) > 1
}

check_data <- function(data) {
  # Sometimes the generator will produce only one response to an item, or
  # several items. You need at least two unique responses for an item to
  # estimate its parameters. This function tests whether all items have at least
  # 2 unique responses.
  
  all(apply(data, 2, check_num_of_unique_responses))
}


simulator <- function(generator, estimator, conditions) {
  output <- generator(conditions)
  data <- output$data
  
  # Check for 2 unique responses for all items. If this test fails, generate
  # a new set of parameters and data and check it again. Do this until the
  # generator creates a valid dataset
  while (!check_data(data)) {
    output <- generator(conditions)
    data <- output$data
  }
  
  true_pars <- output$pars
  est_pars <- estimator(data)
  
  person_result <- data.frame(
    "type" = "ability",
    "true"=true_pars$latent_ability, 
    est_pars$latent_ability,
    c(conditions)
  )
  
  item_result <- data.frame(
    "type" = "difficulty",
    "true"=true_pars$item_difficulty, 
    est_pars$item_difficulty,
    c(conditions)
  )
  
  result <- rbind(person_result, item_result)
  
  return(result)
}

sample_sizes <- c(50, 100, 500)
test_lengths <- c(5, 10, 20)
n_iters <- 500
simulation_conditions <- expand.grid(
  n_persons=sample_sizes,
  n_items=test_lengths
)

n_conditions <- nrow(simulation_conditions)

simulation_results <- data.frame()

for (c in 1:n_conditions) {
  for (i in 1:n_iters) {
    conditions <- simulation_conditions[c,]
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

results_filename <- "results/onePL-simulation-results.csv"
write.csv(simulation_results, results_filename, na="", row.names=FALSE)


# analyze results ####

results_summary <- simulation_results %>%
  mutate(
    bias = true - est,
    ll = est - 1.96*SE,
    ul = est + 1.96*SE
  ) %>%
  group_by(type, n_persons, n_items) %>%
  summarize(
    n_sim = n(),
    mean_bias = mean(bias),
    mean_bias_se = sum(bias^2) / (n_sim * (n_sim-1)),
    rmse = sqrt(mean(bias^2)),
    mean_se = sqrt(mean(SE^2)),
    coverage = mean(ll < true & true < ul),
    r = cor(true, est)
  )

results_summary %>%
  mutate(n_items = factor(n_items)) %>%
  ggplot(aes(x = n_persons, y = mean_se, color=n_items)) +
  geom_line() +
  facet_grid(~ type)

results_summary %>%
  mutate(n_items = factor(n_items)) %>%
  ggplot(aes(x = n_persons, y = r, color=n_items)) +
  geom_line() +
  facet_grid(~ type)





