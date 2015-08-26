# Reads a csv containing data describing hospitals across the US and
# returns the best hospital for a specific outcome (heart attack, heart failure, or pneumonia)

best <- function(state, outcome) {
  ## Read outcome data
  df <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  #lists to reference inputs against
  us_states <- list("AK","AL","AR","AZ","CA","CO","CT","DC","DE","FL","GA","HI","IA","ID","IL","IN","KS","KY","LA","MA","MD","ME","MI","MN","MO","MS","MT","NC","ND","NE","NH","NJ","NM","NV","NY","OH","OK","OR","PA","RI","SC","SD","TN","TX","UT","VA","VT","WA","WI","WV","WY")
  disease <- list("heart attack", "heart failure", "pneumonia")

  ## produce error messages for invalid inputs
  if (missing(state))
      stop("No state specified. Please enter a state.")
  if (missing(outcome))
      stop("Please specify an outcome")
  if (state %in% us_states == "FALSE")
      stop("Error: invalid state")
  if (outcome %in% disease == "FALSE")
      stop("Error: invalid outcome")
  
  
  ## create new dataframe that filters the data source by state
  er <- subset(df, df[7]==state)
  
  # the input "outcome" needs to refer to the appropriate column number
  if (outcome =="heart attack"){
    outcome <- er[11]
  } else if (outcome == "heart failure"){
    outcome <- er[17]
  } else if (outcome == "pneumonia"){
    outcome <- er[23]
  }
  
  for (i in outcome) {
    outcome <- suppressWarnings(as.numeric(i)) #final result after trial & error
  }
  er <- er[complete.cases(er),]
  
  # after conversion, outcome can be used to subset/slice a new dataframe,
  # which is sorted by hospitals' 30-day death rate
  er2 <- er[order(outcome, er["Hospital.Name"]),]
  
  # Return hospital name in that state with lowest 30-day death rate
  winner <- er2[1, "Hospital.Name"]
  return(winner)
}

