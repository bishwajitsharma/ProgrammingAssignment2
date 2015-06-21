## Put comments here that give an overall description of what your
## functions do
## makeCacheMatrix() creates an object of matrix which can store a matrix
## as well as its inverse. cacheSolve() is the equivalent of solve()
## which uses the cacheMatrix object to compute the inverse if not yet 
## computed, else simply returns the computed inverse already cached, 
## thus reducing the computation time.

## Write a short comment describing this function

## makeCacheMatrix: creates a special object containing a matrix and
##  its inverse. If M is a matrix, cM = makeCacheMatrix(M) creates a
##  new object of this special matrix. cM$setmat(newM) can be used to 
##  modify the contents of this special matrix object, in which case 
##  the previously computed inverse is discarded. cM$getmat() returns
##  the original matrix, and cM$getinv() returns the cached inverse,
##  or NULL if inverse was not yet computed. 
##  CAUTION: setinv() should be carefully used. It can overwrite 
##  a pre-computed inverse.

makeCacheMatrix <- function(x = matrix()) {
  invx <- NULL
  setmat <- function(y) {
    x <<- y
    invx <<- NULL # Ensure the inverse is destroyed 
  }
  getmat <- function() x
  setinv <- function(invertedx) invx <<- invertedx 
  getinv <- function() invx
  list(setmat = setmat, getmat = getmat, setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function

## cacheSolve: It performs the same functions as solve() for the special
##  cachedMatrix  object created by makeCacheMatrix(). If called for 
##  first time after cM = makeCacheMatrix(M), invM = cacheSolve(cM)
##  computes the inverse and returns it, and also caches the inverse.
##  In subsequent calls, the cached value is returned.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  invx <- x$getinv()         # Check if the previous result exists
  if(!is.null(invx)){
    message("Fetching cached result of solve")
    return(invx)             # Return the cached result!
  }
  sqmat <- x$getmat()        # Result was not computed yet
  invx <- solve(sqmat, ...)  # so compute it now
  x$setinv(invx)             # and cache it.
  invx
}
