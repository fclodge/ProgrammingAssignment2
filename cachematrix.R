## These functions invert and store the inversion of a matrix to be called from cached storage.

## This function creates the special matrix that will be inverted with the following function.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
      x <<- y
      i <<- NULL
  }
  get <- function() {x}
  setInverse <- function(Inverse) {i <<- Inverse}
  getInverse <- function() {i}
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}



## This function inverts the matrix created in the above function. 
## It then calls the inverted function from the cache if it has previously been inverted.

cacheSolve <- function(x, ...) {
  i <- x$getInverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setInverse(i)
  i
}

