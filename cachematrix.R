## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##function that sets the value and gets the value of the matrix, and also of the inverse.
makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {  ##set the value of the matrix
            x <<- y
            m <<- NULL
      }
      get <- function() x ##get the value of the matrix
      setInverse <- function(inverse) m <<- inverse ##set the value of the inverse
      getInverse <- function() m ##get the value of the inverse
      list(set = set, get = get,
           setInverse = setInverse,
           getInverse = getInverse)
}


## Write a short comment describing this function
##This function computes the inverse of the matrix
cacheSolve <- function(x, ...) {
      m <- x$getInverse()
      if(!is.null(m)) { ##if the inverse is already calculated
            message("getting cached data")
            return(m)
      }
      data <- x$get() ##compute the inverse if it is not calculated before (is not in the cache)
      m <- solve(data, ...)
      x$setInverse(m)
      m
        ## Return a matrix that is the inverse of 'x'
}
