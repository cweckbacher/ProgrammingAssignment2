## Assignment: Caching the Inverse of a Matrix

## This function creates a special "matrix" object that can cache its inverse.



makeCacheMatrix <- function(x = matrix()) {
        ## list of functions that contains:
        # set the value of the vector
        # get the value of the vector
        # set the value of the mean
        # get the value of the mean

  inv = NULL

  set = function(y) {
    x <<- y
    inv <<- NULL
  }

  get = function() x
  setinv = function(inverse) inv <<- inverse
  getinv = function() inv

  return(list(set=set, get=get, setinv=setinv, getinv=getinv))

}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

  inv = x$getinv()

  # check if calc of inverse was already performed
  if (!is.null(inv)){
  # retrieve and skip
    return(inv)
  }

  # clac the inv
  mat.data = x$get()
  inv = solve(mat.data, ...)

  # use the set fct from above
  x$setinv(inv)

  return(inv)

}
