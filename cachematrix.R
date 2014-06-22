#####################
## Assumption: inverse function is already in your environment
######################

## This Function create a special matrix object x, 
## It caches the inverse of this object into memory

makeCacheMatrix <- function(x = matrix()) {
  z <- NULL
  set <- function(y) {
    x <<- y
    z <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) z <<- inverse
  getinverse <- function() z
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse

cacheSolve <- function(x, ...) {
   ## This function compute inverse of a matrix returned by makeCacheMatrix
   ## If Inverse exist and matrix does not change inverse will be retrieved from memory
 
   z <- x$getinverse()
  if(!is.null(z)) {
    message("getting cached data")
    return(z)
  }
  data <- x$get()
  z <- solve(data, ...)
  x$setinverse(z)
  z
}
