  ## Returns the mean of the pollutant across all monitors listed
  ## in the 'id' vector (ignoring NA values)
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
pollutantmean <- function(directory, pollutant, id = 1:332) {
  xfiles <- list.files("C:\\Users\\Pearl\\Documents\\r\\specdata", full.names = TRUE) 
  xfiles2 <- xfiles[id]
  
  garganta <- do.call(
    "rbind",lapply(xfiles2, FUN=function(files)
      {read.csv(files, header=TRUE, sep=",")}))
  
  mean(garganta[, pollutant], na.rm = TRUE)
}
