makeCacheMatrix <- function(x = matrix()) {
  ## set the value of the matrix
  value <- NULL
  set <- function(y) {
    x <<- y
    value <<- NULL
  }
  ## get the value of the matrix
  get <- function() x
  ## set the inverse of the matrix
  setinverse <- function(solve) value <<- solve
  getinverse <- function() value
  ## get the inverse of the matrix
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## inverse a matrix, if the matrix is inversed already, return the cached-inversed one, 
## otherwise, inverse it and save the inversed matrix in cache and return it

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  ## get the inverse of the matrix  
  value <- x$getinverse()
  ## check if there is the matrix  
  if(!is.null(value)) {
    message("getting cached data")
    return(value)
  }
  ## if not: get the inverse of the matrix  
  data <- x$get()
  value <- solve(data, ...)
  ## set the inverse of the matrix 
  x$setinverse(value)
  value
}


##Check function
##RUN
## a<-makeCacheMatrix()
## a$set(matrix(1:4,2,2))
## cacheSolve(a)
