## Put comments here that give an overall description of what your
## functions do


## Function to cache various values of the matrix:
##   1) Set matrix values
##   2) Get matrix values
##   3) Set matrix inverse values
##   4) Get matrix inverse values

makeCacheMatrix <- function(MVal = matrix()) {

     ## Initialize matrix inverse value to NULL
     MInv <- NULL

     ## Set the value of the matrix to a new value. Since new value,
     ## initialize the matrix inverse to NULL.
     setMVal <- function (y) {
          MVal <<- y
          MInv <<- NULL
     }

     ## Return the value of the matrix
     getMVal <- function() {
          MVal
     }
     
     ## Set the value of the matrix inverse (using an argument, not computed)
     setMInv <- function(invValue) {
          MInv <<- invValue
     }

     ## Return the value of the matrix inverse
     getMInv <- function() {
          MInv
     }
     
     list(setMVal = setMVal, getMVal = getMVal, 
          setMInv = setMInv, getMInv = getMInv)
}



## Write a short comment describing this function

cacheSolve <- function(x, ...) {

     ## Get the cached value of the inverse
     s <- x$getMInv()
     
     ## If the cached value is not NULL, return that value
     if (!is.null(s)) {
          message("Getting cached value of inverse")
          return(s)
     }
     
     ## If NULL, get the value of the cached matrix
     data <- x$getMVal()
     
     # calculate the inverse of the data matrix
     s <- solve(data)

     # Set the value of the matrix inverse to the cached matrix
     x$setMInv(s)
     
     # Return the matrix inverse s
     s
}
