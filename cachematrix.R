## Program assignment 2
##
## makeCacheMatrix and cacheSolve functions 
## are used to cache time-consuming matrix inversion computations
##

##
## makeCacheMatrix function creates a special "matrix" object 
## that can cache its inverse. which is really a list containing 
## the following functions
##
## set: set the value of the matrix
## get: get the value of the matrix
## setSolve: set the value of the inverse matrix
## getSolve: get the value of the inverse matrix
##
makeCacheMatrix <- function(x = matrix()) {
  solved <- NULL
  
  # function to reset matrix content
  set <- function(y) {
    x <<- y
    solved <<- NULL
  }
  
  # function to get matrix
  get <- function() x
  
  # function to set solved inverse matrix
  setSolve <- function(invMatrix) solved <<- invMatrix
  
  # function to get cache inverse matrix
  getSolve <- function() solved
  
  # 
  list(set = set, get = get,
       setSolve = setSolve,
       getSolve = getSolve)
}


##
## cacheSolve function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already 
## been calculated and the matrix has not changed, then the cachesolve 
## should retrieve the inverse from the cache.
##
cacheSolve <- function(x, ...) {
  # if already solved, return from cache
  m <- x$getSolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  # solve inverse matrix
  matrix <- x$get()
  invMatrix <- solve(matrix, ...)
  x$setSolve(invMatrix)
  invMatrix
}