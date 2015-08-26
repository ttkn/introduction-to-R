# Similar to best.R, reads a csv containing data describing hospitals across the US, but instead
# expands the scope by returning the specific rank of a hospital (best, worst, 2nd, 3rd, etc...)
# for a specific outcome (heart attack, heart failure, or pneumonia)


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
  er <- subset(df, df[7]==state)
  
  # the input "outcome" needs to refer to the appropriate column number
  if (outcome =="heart attack"){
    outcome <- er[11]
  } else if (outcome == "heart failure"){
    outcome <- er[17]
  } else if (outcome == "pneumonia"){
    outcome <- er[23]
  }
  
  # the outcome columns need conversion from character 
  # to numeric in order to have proper sorting
  for (i in outcome) {
    outcome <- suppressWarnings(as.numeric(i)) #final result after trial & error
  }
  er <- er[complete.cases(er),]
  if (num == "best")
    num <- 1
  
  # after conversion, outcome can be used to subset/slice a new dataframe,
  # if "worst" was given for num, reverse order is used
  # otherwise, hospitals are sorted by mortality rates followed by name
  # and the rank corresponding to num is returned
  if (num == "worst"){
    er2 <- er[order(-xtfrm(outcome), er["Hospital.Name"]),]
    winner <- er2[1, "Hospital.Name"]
    winner
  } else {
    er2 <- er[order(outcome, er["Hospital.Name"]),]
    winner <- er2[num, "Hospital.Name"]
    winner
  }
}
