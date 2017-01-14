
## Functions for creating and using inverted matrices which caching possibility

#-----------------------------------------------------------------------------------
## Creates cacheable matrix for inputting to
## cacheSolve() function which sets and gets 
## the cached values
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  # Functions for getting and setting cached inv. matrix value
  get <- function() x
  # Inversing the matrix using build in solve() function in R
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}
#-----------------------------------------------------------------------------------

#-----------------------------------------------------------------------------------
## Computes the inverse of the cacheable matrix returned by makeCacheMatrix()
## If the inverse has already been calculated and there's no change in the matrix
## then the cacheSolve() returns the cached inverse matrix save
cacheSolve <- function(x, ...) {
  m <- x$getinv()
  # Do we have a change in the matrix ?
  if(!is.null(m)) {
    message("The matrix did not change")
    # Return the matrix inverse saved
    m
  }
  # Let's create inverted matrix in case that it changed
  else{
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    # Return the matrix inverse changed
    m
  }
}
#-----------------------------------------------------------------------------------
