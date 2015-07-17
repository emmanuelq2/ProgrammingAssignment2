
## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(m = matrix()) {
  ## define the cache inv
  inv <- NULL
  set <- function(y) {
  ## assign the input matrix y to the variable m in the parent environment
	m <<- y
	inv <<- NULL
  }
  ## return the matrix m
  get <- function() m
  ## set the cache inv equal to the inverse of the matrix with the solve function nested into a parameterized function 
  setInverse <- function(solve) inv <<- solve
  ## return the cached inverse of m
  getInverse <- function() inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated (and the
## matrix has not changed), then cachesolve should retrieve the inverse 
## from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv
}