## Matrix inversion is usually a costly computation 
## and there may be some benefit to caching the inverse of a matrix 
## rather than computing it repeatedly 
## (there are also alternatives to matrix inversion that we will not discuss here). 
## The assignment is to write a pair of functions that cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
## Function to set the value of the matrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
## Function to get the value of the matrix
  get <- function() {
    x
  }
## Function to set the inverse of the matrix
  setinverse <- function(inverse) {
    m <<- inverse
  }
## Function to get the inverse of the matrix
  getinverse <- function() {
    m
  }
## Return a list of the Functions
  list(set = set, 
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
## Return a reversed matrix of x
  m <- x$getinverse()
## if the inverse has been calculated, return it from the cached data
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
## Get the martrix
  data <- x$get()
## Calculate the inverse
  m <- solve(data, ...)
## Set the inverse to the object
  x$setinverse(m)
  ## Return a matrix that is the inverse of 'x'
  m
}