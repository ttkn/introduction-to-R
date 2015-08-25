  ## 'directory' is a character vector of length 1 indicating the location of the CSV files
  ## 'threshold' is a numeric vector of length 1 indicating the number of completely 
  ## observed observations (on all variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  ## Return a numeric vector of correlations

corr <- function(directory, threshold = 0){
  xfiles <- list.files("C:\\resolve\\introduction-to-R\\specdata", full.names = TRUE)
  
  pain <- vector(mode="numeric", length=0)
  
  e <- function(x)
  {nrow(na.omit(read.csv(x, header=TRUE, sep=",")))}
  
  e2 <- function(x){read.csv(x, header=TRUE, sep=",")}

  e3 <- function(x){cor(e2(x)["nitrate"], e2(x)["sulfate"], use="p")}
        
  for(i in xfiles){
    if(e(i)<threshold)
      next
    pain[i] <- e3(i)
       }
  return(pain)
}
