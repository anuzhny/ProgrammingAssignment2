## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  value <- NULL
  set <- function(y) {
    x <<- y
    value <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) value <<- solve
  getinverse <- function() value
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  value <- x$getinverse()
  if(!is.null(value)) {
    message("getting cached data")
    return(value)
  }
  data <- x$get()
  value <- solve(data, ...)
  x$setinverse(value)
  value
}
