## The following functions can cache the inverse of a matrix

## This function, makeCacheMatrix, creats a special object that
## stores a matrix.

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setsolve <- function(solve) m <<- solve
      getsolve <- function() m
      list(set = set, get = get,
           setsolve = setsolve,
           getsolve = getsolve)
}

## This function, cacheSolve, gets the inversed matrix from the 
## cache and skips the computation if the matrix has already been
## inversed. Otherwise, it calculates the inverse of the matrix and
## sets the value in the cache via the setsolve function.

cacheSolve <- function(x, ...) {
      m <- x$getsolve()
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$setsolve(m)
      m
}
