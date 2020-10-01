## makeCacheMatrix is a function that creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## To compute the inverse of the matrix that is returned by makeCacheMatrix above

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  ##To check if the inverse is already calculated
  if (!is.null(inv)) {
    message("getting cached data")
    ##the inverse is retrieved from the cache
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}


