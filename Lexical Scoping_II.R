
## Supposing than matrix has ever inverse
## we have a pair of functions that cache the inverse of a matrix.


##This function "makeCacheMatrix" gets a matrix as an input, set the value of the matrix,
#get the value of the matrix, set the inverse Matrix and get the inverse Matrix.
makeCacheMatrix <- function(x = matrix()) {
  
      invMatrix <- NULL
  
      #set the value of the Matrix
      setMatrix <- function(y) {
            x <<- y
            invMatrix <<- NULL
      }
  
      getMatrix <- function() x                              #get the value of the Matrix
  
      setInverse <- function(inverse) invMatrix <<- inverse  #set the value of the inverse
  
      getInverse <- function() invMatrix                     #get the value of the inverse
  
      list(setMatrix = setMatrix, getMatrix = getMatrix,
            setInverse = setInverse, getInverse = getInverse)
  
}


## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix above.
cacheSolve <- function(x, ...) {
      
      m <- x$getinv()
      
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      
      data <- x$get()
      
      m <- solve(data, ...)
      
      x$setinv(m)
      
      return(round(m,digits=2))
}