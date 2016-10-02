## Put comments here that give an overall description of what your
## functions do

# makeCacheMatrix and cachSolve are a pair of functions that cache the inverse of a matrix 
# assumed to be always invertible.


## Write a short comment describing this function

# makeCacheMatrix stores a pecial "matrix" object that can cache its inverse
# The special matrix object contains a list containing a function to:
# 1. set matrix
# 2. get matrix
# 3. set the inverse 
# 4. get the inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- solve
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function

# cacheSolve computes the inverse of the special matrix output by makECacheMatrix. If, however,
# the inverse has already been calculated or computed, cacheSolve retrieves the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if (!is.null(inv)) {
    message("getting cached matrix data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data,...)
  x$setinv(inv)
  inv
}
