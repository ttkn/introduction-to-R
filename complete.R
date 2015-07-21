complete <- function(directory, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  bfiles <- list.files("C:\\resolve\\introduction to R\\specdata", full.names = TRUE)
  bfiles2 <- bfiles[id]
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  e <- function(x)
    {nrow(na.omit(read.csv(x, header=TRUE, sep=",")))}
  df <- data.frame()
  colnames(df) <- c("id", "nobs")
  for (i in bfiles2){
    
  }
  (lapply(bfiles2, e))
  
  return(df)
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases
}