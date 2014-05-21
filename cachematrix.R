## These two functions create a special matrix that can cache its inverse
## in order to avoid doing the costly calcuation of the finding inverse of
## a matrix, after it has already been calculated once.

## This function creates a special "matrix" object that can cache its inverse.
## Arguments:
## x - the matrix to initialize this special matrix with

makeCacheMatrix <- function(x = matrix()) {
   i <- NULL
   
   set <- function(y) {
      x <<- y
      i <<- NULL
   }
   get <- function() x
   setinverse <- function(inverse) i <<- inverse
   getinverse <- function() i
   
   list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function computes the inverse of the special
## "matrix" returned by `makeCacheMatrix`. If the inverse has
## already been calculated (and the matrix has not changed), then
## `cacheSolve` should retrieve the inverse from the cache.
## Arguments:
## x - the special matrix to return the inverse for

cacheSolve <- function(x, ...) {
   ## Return the cached inverse if available
   i <- x$getinverse()
   if(!is.null(i)){
      message("getting cached data")
      return(i)
   }
   ## Calculate the inverse, cache, and return
   data <- x$get()
   i <- solve(data, ...)
   x$setinverse(i)
   i
}
