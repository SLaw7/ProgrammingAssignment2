## Together, the following functions output the inverse 
## of an invertable matrix. If the inverse matrix has 
## already been computed, then these functions will avoid 
## recalulating the inverse.

## makeCacheMatrix makes a list of four functions that 
## either "set" or "get" the original matrix or the 
## inverse matrix.

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


## cacheSolve will immediately output an inverse matrix, 
## if it has already been computed. Otherwise, it computes
## the inverse matrix and stores the inverse for future 
## reference.

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
        ## Return a matrix that is the inverse of 'x'
}
