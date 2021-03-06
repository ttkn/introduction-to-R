  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the number of complete cases
  ## 'directory' is a character vector of length 1 indicating the location of the CSV files
  
complete <- function(directory, id = 1:332) {
 
  bfiles <- list.files("C:\\resolve\\introduction to R\\specdata", full.names = TRUE)
  bfiles2 <- bfiles[id]

  e <- function(x)
    {nrow(na.omit(read.csv(x, header=TRUE, sep=",")))}
 
  df <- data.frame()
 
  for (i in bfiles2){
    id <- list(read.csv(i)[1, "ID"])
    nobs <- list(e(i))
    # combine the two lists as columns, then combine the result as rows with df,
    # making a new dataframe that has all the station ids and their nobs
    df <- rbind(df, cbind(id, nobs))  
  }
 return(df)
}
