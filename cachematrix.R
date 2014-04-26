## This pair of functions cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  CacheMatrix <- NULL
  # Set the value of the matrix
  set <- function(y) {
    x <<- y
    CacheMatrix <<- NULL
  }
  # Get the value of the matrix
  get <- function() x
  # Set the value of the inverse
  setsolve <- function(solve) CacheMatrix <<- solve
  # Get the value of the inverse
  getsolve <- function() CacheMatrix
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## The following function calculates the inverse of the special 'matrix' created with the above function. 
## However, it first checks to see if the inverse has already been calculated. If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse and sets the value of the inverse in the cache via the setsolve function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  CacheMatrix <- x$getsolve()
  if(!is.null(CacheMatrix)) {
    message("getting cached data")
    return(CacheMatrix)
  }
  data <- x$get()
  CacheMatrix <- solve(data, ...)
  x$setsolve(CacheMatrix)
  CacheMatrix
}
