## Creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  ## Matrix inverse.
  i <- NULL
  
  ## Setter/getter for matrix data.
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  
  ## Setter/getter for matrix inverse.
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  
  ## Return a list of all setters/getters.
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}



## Computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated
## (and the matrix has not changed), then the cachesolve should
## retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  ## Get inverse from cache if it has been computed before.
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  
  ## Compute matrix inverse since it has not been computed before.
  ## Use the solve function to compute matrix inverse.
  data <- x$get()
  i <- solve(data, ...)
  
  ## Cache the computed matrix inverse and return it.
  x$setinverse(i)
  i
}