## This two functions create a special object, a matrix, that computes the inverse of a matrix and caches it.

## This function creates a matrix that cache its own inverse, so it does not have to be calculated
## on each computation. It allowes to set and get the value of the matrix, and set and get the value
## of its inverse.

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


## This function computes the matrix inverse of the matrix created by function "makeCacheMatrix",
## it retrieves the inverse from the cache.

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
        ## Return a matrix that is the inverse of 'x'

cacheSolve(makeCacheMatrix(x))
