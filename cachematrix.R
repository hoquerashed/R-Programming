## Matrix inversion is usually a costly computation.
## The following functions "makeCacheMatrix" and "cacheSolve" together implement a caching strategy
## which is to cache "inverse matrix" once calcualted (rather than compute it repeatedly). 
## Please note, there is no special consideration for cases where inverse matrix calcualtion is not possible.

## The function "makeCacheMatrix" creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  
  set <- function(m) {
    x <<- m
    inverse <<- NULL
  }
  get <- function() x
  
  setinverse <- function(inv) inverse <<- inv
  getinverse <- function() inverse
  
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## The function "cacheSolve" computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}