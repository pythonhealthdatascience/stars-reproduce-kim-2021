# Function to count the aorta sizes of people with AAA-related deaths
# in surv scenario 0 and 1

get_aaa_death_aorta_sizes <- function(df, n, period) {
  #' Get a count of the aorta sizes for people who had AAA-related deaths
  #' 
  #' Counts the number of small (3.0-4.4cm), medium (4.5-4.9cm) and large
  #' (5.0-5.4cm)
  #' 
  #' @param df Output from the model
  #' @param n Number of people in simulation
  #' @param period Number of years that ultrasound scans are suspended
  #' 
  #' @return aaa_deaths_aorta_size Dataframe with counts of each size
  
  # Get a list of aorta sizes from people who had AAA-related deaths
  aorta <- c()
  for (person in df$eventHistories) {
    if ("aaaDeath" %in% person$screening$events) {
      aorta <- c(aorta, person$screening$initialAortaSizeAsMeasured)
    }
  }
  
  # Count the number in each of the size groups
  aorta_small <- sum(aorta >= 3 & aorta <= 4.4)
  aorta_med <- sum(aorta >= 4.5 & aorta <= 4.9)
  aorta_large <- sum(aorta >= 5 & aorta <= 5.4)
  
  # Convert into a dataframe
  aaa_deaths_aorta_size <- data.frame(
    aorta_size = c("small", "med", "large"),
    aaadead = c(aorta_small, aorta_med, aorta_large)
  )
  aaa_deaths_aorta_size$n <- n
  aaa_deaths_aorta_size$period <- period
  
  return (aaa_deaths_aorta_size)
}