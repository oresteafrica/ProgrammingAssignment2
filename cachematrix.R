## Below are two functions that create a special object
## that store a matrix and cache's its inverse

## Creates a special matrix containing a functions to
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse
## get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  inverse  <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() { x }
  setinverse <- function(inv){ inverse <<- inv }
  getinverse <- function() { inverse } 
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse) 
}

## Return the inverse of a matrix

cacheSolve <- function(x, ...) {
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
}
