## Caches the Inverse of a Matrix

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) { # sets x equal to input and m equal to NULL
    x <<- y
    m <<- NULL
  }
  get <- function() x # returns x
  setinv <- function(inv) m <<- inv # assigns inv to m
  getinv <- function() m # returns inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function computes the inverse of the special "matrix returned by 
## makeCacheMatrix. If the inverse has already been calculated (and the matrix
## has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinv() # gets inverse
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get() # gets data to invert
  m <- solve(data, ...) # inverts matrix
  x$setinv(m) # assigns inv to cached variable
  m
}
