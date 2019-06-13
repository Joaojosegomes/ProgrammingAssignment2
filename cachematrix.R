## Let us assume that the matrix has inverse
## We have a pair of functions that cache the inverse of a matrix.


##This function "makeCacheMatrix" gets a matrix as an input, set the value of the matrix,
#get the value of the matrix, set the inverse Matrix and get the inverse Matrix.
makeCacheMatrix <- function(x = matrix()) {
      invMat <- NULL
      #set the value of the Matrix
      setMat <- function(y) {
            x <<- y
            invMat <<- NULL
      }
      getMat <- function() x                             
      setInv <- function(inverse) invMat <<- inverse  
      getInv <- function() invMat
      list(setMat = setMat, getMat = getMat,
           getInv = getInv, setInv = setInv)
}


## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix above.
cacheSolve <- function(x, ...) {
      m <- x$getInv()
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      data <- x$getMat()
      m <- solve(data, ...)
      x$setInv(m)
      return(m)
}
