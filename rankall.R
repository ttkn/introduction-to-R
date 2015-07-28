rankall <- function(outcome, num = "best") {
  
  df <- read.csv("outcome-of-care-measures.csv")
  df[11] <- suppressWarnings(as.numeric(levels(df[,11])[df[,11]]))
  df[17] <- suppressWarnings(as.numeric(levels(df[,17])[df[,17]]))
  df[23] <- suppressWarnings(as.numeric(levels(df[,23])[df[,23]]))
  
  #lists to reference inputs against
  disease <- list("heart attack", "heart failure", "pneumonia")
  
  ## produce error messages for invalid inputs
  if (missing(outcome))
    stop("Please specify an outcome")
  if (outcome %in% disease == "FALSE")
    stop("Error: invalid outcome")
  
  # the input "outcome" needs to refer to the appropriate column number
  if (outcome =="heart attack"){
    outcome <- 11
  } else if (outcome == "heart failure"){
    outcome <- 17
  } else if (outcome == "pneumonia"){
    outcome <- 23
  }
  
  er <- split(df, df$State) #the list of sub-dataframes will be iterated over below
  
  if (num == "best")
    num <- 1
  
  result <- matrix(nrow=0, ncol=2)
  
  # if "worst" was given for num, reverse order is used
  # otherwise, hospitals are sorted by mortality rates followed by name (sortie)
  if (num == "worst"){
    for (i in er){
      sortie <- i[order(-xtfrm(i[outcome]), i["Hospital.Name"]),]
      result <- rbind(result, c(as.character(sortie[1,2]), as.character(sortie[1,7])))
          }
  } else {
      for (i in er){
        sortie <- i[order(i[outcome], i["Hospital.Name"]),]
        result <- rbind(result, c(as.character(sortie[num,2]), as.character(sortie[num,7])))
      }#this rbind appends 2 values to the result matrix defined earlier
  }
  colnames(result) <- c("hospital", "state")
  result <- as.data.frame(result)
  return(result)
}