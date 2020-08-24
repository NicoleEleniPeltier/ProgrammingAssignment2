### cachematrix.R ###
### Nicole Peltier ###
### August 24, 2020 ###

### Calculates and caches inverse of matrix to save time on
###   repeated calls for inverse
### First, initialize on matrix x: x_matrix <- makeCacheMatrix(x)
### Next, get inverse of matrix: cacheSolve(x_matrix)
### Repeated calls of cacheSolve will use cached value of inverse

### makeCacheMatrix ###
# Input: matrix x
# Caches matrix inverse to save computation time by not having
#   to recalculate every time it is called
# Creates set/get functions for matrix and inverse
makeCacheMatrix <- function(x = matrix()) {
  # Initialize inverse to be NULL
  inverse <<- NULL
  
  # Set/get functions for x
  set <- function(y){
    x <<- y
    # Every time x is changed, reset inverse to NULL
    inverse <<- NULL
  }
  get <- function() x
  
  # Set and get inverse of matrix
  set_inverse <- function(inv) inverse <<- inv
  get_inverse <- function() inverse
  
  # Return list that contain set/get functions for x and inverse
  list(set = set,
       get = get,
       set_inverse = set_inverse,
       get_inverse = get_inverse)

}

### cacheSolve ###
# Input: matrix x
# Returns matrix that is inverse of x
# If inverse already solved/cached, returns cached inverse
# If no cached value saved, calculates inverse
cacheSolve <- function(x, ...) {
  # First, look for value already set
  inverse <- x$get_inverse()
  
  # If inverse already cached, return cached value
  if(!is.null(inverse)){
    message("Already calculated! Getting cached data")
    return(inverse)
  }
  
  # If inverse not cached, calculate
  message("Not calculated yet... calculating now")
  data <- x$get()
  inverse <- solve(data, ...)
  x$set_inverse(inverse)
  
  # Return inverse
  inverse
}
