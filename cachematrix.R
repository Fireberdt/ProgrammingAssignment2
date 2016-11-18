# makeCacheMatrix creates a matrix which is actually a list containing a 
# function that can set/get the values of the matrix and set/get the values
# of the inverse. 

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL 
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


# cacheSolve solves for the inverse of a matrix but if that inverse of that 
# matrix is already found then the cached value is returned instead.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("Getting  cached data.")
    return(inv)
  }
  result <- x$get()
  inv <- solve(result)
  x$setInverse(inv)
  return(inv)
}
