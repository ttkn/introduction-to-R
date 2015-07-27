# rankhospital returns the name of a hospital whose mortality
# rate for a given disease is the highest, lowest, 
# or other rank in a given state

rankhospital <- function(state, outcome, num = "best") {
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
  er <- subset(df, df[7]==state 
               & df[11] != "Not Available"
               & df[17] != "Not Available"
               & df[23] != "Not Available")
  
  # the input "outcome" needs to refer to the appropriate column number
  if (outcome =="heart attack"){
    outcome <- 11
  } else if (outcome == "heart failure"){
    outcome <- 17
  } else if (outcome == "pneumonia"){
    outcome <- 23
  }
  
  if (num == "best")
    num <- 1
  
  # after conversion, outcome can be used to subset/slice a new dataframe,
  # if "worst" was given for num, reverse order is used
  # otherwise, hospitals are sorted by mortality rates followed by name
  # and the rank corresponding to num is returned
  if (num == "worst"){
    er2 <- er[order(-xtfrm(er[outcome]), er["Hospital.Name"]),]
    winner <- er2[1, "Hospital.Name"]
    winner
  } else {
    er2 <- er[order(er[outcome], er["Hospital.Name"]),]
    winner <- er2[num, "Hospital.Name"]
    winner
  }
}