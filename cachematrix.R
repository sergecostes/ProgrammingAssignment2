## Description:
# > x <- matrix(rnorm(20000), nrow = 100000) -- Create a matrix x
# > cx <- makeCacheMatrix(x) -- Create our cache object to store an inverse matrix
# > cx$get() -- Return the matrix
# > cacheSolve(cx) -- Return the inverse
# > cacheSolve(cx) -- 2nd call uses cache


## makeCacheMatrix: return a list of functions to:
# 1. Set the value of the matrix
# 2. Get the value of the matrix
# 3. Set the value of the inverse
# 4. Get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
  
  # cache for the inverse matrix
  invm <- NULL
  
  # 1. Set a matrix and reset cache
  set <- function(y) {
    x <<- y
    invm <<- NULL
  }
  
  # 2. Get the matrix
  get <- function() x
  
  # 3. Set the inverse
  setinvm <- function(inversematrix) invm <<- inversematrix
  
  # 4. Get the inverse
  getinvm <- function() invm
  
  list(set = set, get = get, setinvm = setinvm, getinvm = getinvm)
}


## cacheSolve: Make the inverse of the matrix. If the inverse was already
# calculated, it returns the cached inverse.
cacheSolve <- function(x, ...) {
  
  invm <- x$getinvm()
  
  # Test if the inverse is already calculated, if yes, return it
  if (!is.null(invm)) {
    
    message("getting cached data")
    return(invm)
  }
  
  # The inverse is not yet calculated, so make it it
  mdata <- x$get()
  invm <- solve(mdata, ...)
  
  # Cache the inverse
  x$setinvm(invm)
  
  # Return it
  invm
}
