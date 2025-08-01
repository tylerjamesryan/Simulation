

# monte hall problem

remove_door <- function(prize_door, first_choice) {
  doors <- 1:3
  # identify which door(s) are not chosen or the prize door
  available_doors <- which(!(doors %in% c(first_choice, prize_door)))
  
  # sample from the available doors to remove
  available_idx <- 1:length(available_doors)
  remove_door <- available_doors[sample(available_idx, 1)]
  
  return(remove_door)
}

simulator <- function() {
  initial_doors <- 1:3
  prize_door <- sample(initial_doors, 1)
  first_choice <- sample(initial_doors, 1)
  
  to_remove <- remove_door(prize_door, first_choice)
  
  remaining_doors <- which(!(initial_doors %in% to_remove))
  switch <- remaining_doors[which(first_choice != remaining_doors)]
  stay <- first_choice
  
  result <- c(prize=prize_door, switch=switch, stay=stay)
  
  return(result)
}

output <- as.data.frame(t(replicate(10000, simulator())))

with(output, mean(prize == switch)) # ~ 2/3
with(output, mean(prize == stay)) # ~ 1/3


