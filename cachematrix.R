## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  
  #set the value of the matrix
  set <- function(y) {
    x <<- as.matrix(y)
    m <<- NULL
  }
  
  #get the value of the matrix
  get <- function() x
  
  #set the value of the solve
  setsolve <- function(solve) m <<- solve
  
  #get the value of the solve
  getsolve <- function() m
  
  list(set = set, get = get,setsolve = setsolve,getsolve = getsolve)
}



## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x,...) {
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  message("caching data")
  x$setsolve(m)
  m
}