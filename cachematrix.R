## This functions either return the inverse of a matrix from
## cache if it has been computed before or do a fresh inverse
## calculation if the computation hasn't been done before.

## This function returns inverse of a matrix. 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y)
  {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(inverse) m <<- inverse
  getsolve <- function() m
  list (set = set, get = get, setsolve = setsolve, getsolve = getsolve)
  
}


## This function checks if the inverse of a matrix 
## has been calculated previously. If yes, cached data
## is restored. Else, function to inverse is used. 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
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
