## The following functions cache the inverse of a matrix

## The function makeCacheMatrix creates a special "matrix", which is a list containing a function to
##	1. set the value of the matrix
##	2. get the value of the matrix
##	3. set the value of inverse of the matrix
##	4. get the value inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  cachedInverse <- NULL
  set <- function(y) {
    x <<- y
    cachedInverse <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) cachedInverse <<- inverse
  getInverse <- function() cachedInverse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## The following function returns inverse of the "special" matrix

cacheSolve <- function(x, ...) {
  invFunc <- x$getInverse()
  if(!is.null(invFunc)) {
    message("getting cached data")
    return(invFunc)
  }
  data <- x$get()
  invFunc <- solve(data, ...)
  x$setInverse(invFunc)
  invFunc
}
