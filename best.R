best <- function(state, outcome) {
  ## Read outcome data
  df <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  us_states <- list("AK","AL","AR","AZ","CA","CO","CT","DC","DE","FL","GA","HI","IA","ID","IL","IN","KS","KY","LA","MA","MD","ME","MI","MN","MO","MS","MT","NC","ND","NE","NH","NJ","NM","NV","NY","OH","OK","OR","PA","RI","SC","SD","TN","TX","UT","VA","VT","WA","WI","WV","WY")
  disease <- list("heart attack", "heart failure", "pneumonia")
  
  ## Check that state and outcome are valid
  if (missing(state))
      stop("No state specified. Please enter a state.")
  
  if (missing(outcome))
      stop("Please specify an outcome")
    
  if (state %in% us_states == "FALSE")
      stop("Error: invalid state")
   
  if (outcome %in% disease == "FALSE")
      stop("Error: invalid outcome")
  
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  }