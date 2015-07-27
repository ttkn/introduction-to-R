pollutantmean <- function(directory, pollutant, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  xfiles <- list.files("C:\\Users\\Pearl\\Documents\\r\\specdata", full.names = TRUE) 
  xfiles2 <- xfiles[id]
  
  garganta <- do.call( #a lucky instance of do.call working correctly b/c of rbind
    "rbind",lapply(xfiles2, FUN=function(files)
      {read.csv(files, header=TRUE, sep=",")}))
  mean(garganta[, pollutant], na.rm = TRUE)
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
  ## NOTE: Do not round the result!
}

