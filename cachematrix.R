## The following makeCacheMatrix function creates a special matrix 
## object that can cache its inverse. It will return a list 
## containing functions that will be used as input in the 
## cacheSolve function.

makeCacheMatrix <- function(x = matrix()) {
  invmat <- NULL
  ##Set the Matrix
  set <- function(y){
  x <<- y
  invmat <<- NULL
  }
  ##Get the Matrix
  get <- function()x
  ##Set the Inverse
  setinvmat <- function(inverse) invmat <<- inverse
  ##Get the Inverse
  getinvmat <- function() invmat
  list(set=set, get=get, setinvmat=setinvmat, getinvmat=getinvmat)
}


## The cacheSolve function takes the output of the makeCacheMatrix
## function and returns the inverse of the original matrix.

cacheSolve <- function(x, ...) {
  invmat <- x$getinvmat()
  ## if the inverse is already calculated and the matrix hasn't changed
  ## the inverse from the cache is retrieved.
  if(!is.null(invmat)){
     message("getting cached data")
     return(invmat)
  }
  ## otherwise, the inverse is calculated below
  data <- x$get()
  invmat <- solve(data, ...)
  x$setinvmat(invmat)
  return(invmat)
}
