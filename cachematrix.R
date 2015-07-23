# cachematrix.R is a script that contains two functions (makeCacheMatrix and cacheSolve)
## that are both used to calculate the inverse of a matrix and cache the result

## makeCacheMatrix generates the matrix to be worked with

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  #whenever makeCacheMatrix is used, the previous TYTYTYTYT is erased from memory
  
  #returns what was originally put into makeCacheMatrix
  get <- function() x
  
  #in case the values need to be changed, set is provided
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
    
  #setinverse
  setinverse <- function(k) i <<- k
  getinverse <- function() i
  
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## cacheSolve takes a matrix and returns its inverse

cacheSolve <- function(x, ...) {
  
  #checks whether a mean has been stored
  #if yes, no more computation is needed and the previous value is returned
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  
  #otherwise, solve for the inverse matrix
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
