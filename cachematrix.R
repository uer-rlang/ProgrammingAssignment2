## Implements caching the inverse of a matrix

## Creates special matrix object that can cache the inverse

makeCacheMatrix <- function(x = matrix()) {
  mInverse <- NULL            # sets default value of inverse matrix

  set <- function(y) {        # define funtion to set value of input matrix
    x <<- y	                  # save input matrix
    mInverse <<- NULL         # because input matrix has changed we reset inverse matrix to null
  }
  get <- function() x		# define function to get value of input matrix

  setInverse <- function(aInv) mInverse<<- aInv # define function to set inverse matrix
  getInverse <- function() mInverse             # define function to get inverse matrix

  list(set = set, get = get, setInverse = setInverse,
             getInverse = getInverse )
}


## Computes the inverse of special matrix object or returns the cached inverse
## matrix if it was already calculated and initial matrix didn't change

cacheSolve <- function(x, ...) {
  mInverse <-x$getInverse()         # get cached inverse matrix and check if it exists
  if( !is.null( mInverse ) ) {      # if it exists - use it as a result inverse matrix
    message("getting cahced data") 
    return( mInverse )
  }
  data <- x$get()                   # if it does not exist - get input matrix
  mInverse <- solve (data, ...)     # compute inverse matrix for given input matrix
  x$setInverse(mInverse)            # save inverse matrix to cache
  mInverse                          # return inverse matrix
}