## The 'makeCacheMatrix' function builds a set of functions and outputs the
## function list to the parent environment.  The list output allows the use
## of the matrix input and inverse via the setter and getters defined in the
## 'makeCacheMatrix' function.  The 'cacheSolve' function takes the input
## from the 'makeCacheMatrix' function then calculates and stores the inverse.
## If an inverse already exists, 'makeCacheMatrix' returns the stored inverse.

## The 'makeCacheMatrix' function builds a set of functions--'set', 'get'
## 'setinverse', and 'getinverse' and returns the functions within a list
## to the parent environment.  The list of functions can be input to the
## 'makeCacheMatrix' function.

makeCacheMatrix <- function(x = matrix()) {
  
  i <- NULL # clears any previous inverse
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) i <<- solve
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The 'cacheSolve' function takes the input from the 'makeCacheMatrix'
## function then calculates the inverse.  The 'cacheSolve' function first
## searches for a previously stored inverse and returns it if the inverse
## exists.  If the inverse does not exist, the 'cachSolve' function calculates
## the inverse and stores as i.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}

## test with m1 <- matrix(c(1/2, -1/4, -1, 3/4), nrow = 2, ncol = 2)
## expected result = matrix(c(6, 2, 8, 4), nrow = 2, ncol = 2)