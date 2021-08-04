## These two functions store a matrix, then calculate and cache
## the inverse of that matrix

## This function creates a special "matrix" object that can cache
## the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL ## resets the inverse to NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  } ## stores the initial matrix
  get <- function() x ## gets the value of the matrix
  setinv <- function(solve) m <<- solve ## sets the value of the
                                        ## inverse of the matrix
  getinv <- function() m ## gets the value of the inverse
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv) ## this creates the "list" containing
                        ## the functions
}


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already
## been calculated (and the matrix has not changed), then the 
## cachesolve will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinv() ## checks to see if the inverse is 
                  ## already calculated
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  } ## if the inverse was already calculated, return cached value
  data <- x$get() ## if not calculated, get initial matrix
  m <- solve(data, ...) ## calculate inverse of initial matrix
  x$setinv(m) ## set the calculated value
  m ## return the calculated inverse
}
