## Put comments here that give an overall description of what your
## functions do

## this function creates an object that stores the matrix data (x)
##the set function sets the inverse (i) initally to be NULL
## setInverse allows the user to store a cached copy of the inverse.
## getInverse simply gets the cached inverse. It is null if it hasn't been copied yet.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## this function looks to see if the inverse has been cached in the matrix object. If yes, it will get it
## otherwise, it will calculate it, and stores it in the cache.
## this function returns a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
