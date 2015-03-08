## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix function creats a special "matrix" object,
## which is really a list containing functions to:
## * set the value of matrix
## * get the value of matrix
## * set the value of the inverse of the matirx
## * get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) inverse <<- solve
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function calculates the inverse of a special "matrix"
## returned by makeCacheMatrix above.
## * If the inverse has been already calculated, and the matrix
##   has not changed, then the inverse of the matrix is retrieved
##   from the cache.
## * If the inverse has not been calculated yet, then the inverse
##   is calculated by getinverse function from makeCacheMatrix.
##   And after that, the inverse is set to the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinverse()
  if(!is.null(inverse)){
    message("getting cached inverse value of the matrix")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data)
  x$setinverse(inverse)
  inverse
  
}
