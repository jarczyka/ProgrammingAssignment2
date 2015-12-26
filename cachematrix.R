## The functions below cache the inverse of a matrix so that it does not need to be 
## recalculated every time you need the solution (e.g., if it is required in a loop)

## This function creates an object to cache a matrix's inverse 
## so it doesn't need to be recalculated unnecessarily 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get, 
       setinverse = setinverse, 
       getinverse = getinverse)
}


## This function returns the inverse of the matrix returned by makeCacheMatrix 
## (If the solution is already stored in the cache, then the function retrieves it
## instead of calculating the solution anew) 

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
