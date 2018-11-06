## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# makeCacheMatrix takes in a matrix as an argument (2x2)
# variable m is set as a local variable to null
# set function takes in the matrix and assigns it to another variable,
# variable m is elavated to a global variable and set to null
# get is a function that stores x
# setinv sets value for m in the global environment
## End of Function

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...) 
  x$setinv(m)
  m
}
 

mymatrix<-matrix(c(20,33,60,80),2,2)
mycachedmatrix<-makeCacheMatrix(mymatrix)
cacheSolve(mycachedmatrix)
