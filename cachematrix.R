## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## The below function makeCacheMatrix() caches a matrix and
## it's inverse. It has 4 functions to set and retreive the
## matrix and its cached inverse
makeCacheMatrix <- function(x = matrix()) {
  invMatrix <- NULL
  set <- function(y) {
    x <<- y
    invMatrix <<- NULL
  }
  get <- function() x
  setInverse <- function(inv) {
    invMatrix <<- inv
  }
  getInverse <- function() invMatrix
  list(set = set, get = get, setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function
## Returns the inverse of the matrix cached in obj x
## If the inverse is already computed (and cached)
## then the cached value is returned, otherwise
## inverse is calculated using solve()
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)) {
    print("getting cached inverse")
    return(inv)
  }
  inv <- solve(x$get(), ...)
  x$setInverse(inv)
  inv
}
