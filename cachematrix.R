## makeCacheMatrix is a function that returns a list of functions.
## this function stores a matrix and caches an inverse of that matrix for later use
## it is relatively smilar to the example code with means swapped for inverses or solves
## Note: used dummy and inv instead of single letter variables to help me better
## organize my thoughts

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(dummy) {
      x <<- dummy
      inv <<- NULL
    }
    get <- function () x
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    list(set = set, get = get, setIverse = setInverse, getInverse = getInverse)
}


## Checks to see if the inverse of the matrix has already been cached
## If so, t will grab the cached and not do any computation
## Otherwise it will compute the inverse and cache it

cacheSolve <- function(x, ...) {
      inv <- x$getSolve()
      if(!is.null(inv)){
        message("getting cached data")
        return(inv)
      }
      data <- x$get()
      inv <- solve(data, ...)
      x$setSolve(inv)
      inv
}
