corr <- function(directory, threshold = 0){
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  # 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  ## NOTE: Do not round the result!
  
  xfiles <- list.files("C:\\users\\Pearl\\documents\\r\\specdata", full.names = TRUE)
  
  pain <- matrix()
  
  e <- function(x)
  {nrow(na.omit(read.csv(x, header=TRUE, sep=",")))}
  
  e2 <- function(x){read.csv(x, header=TRUE, sep=",")}

  e3 <- function(x){cor(e2(x)["nitrate"], e2(x)["sulfate"], use="complete.obs")}
        
  for(i in xfiles){
    if(e(i)>threshold)
      {
      pain <- rbind(pain, lapply(i, e3))
       }}
  return(pain)
}
