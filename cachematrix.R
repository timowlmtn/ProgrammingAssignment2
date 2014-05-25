# This program demonstrates the use caching in R to create a data structure where the inverse is 
# only computed as needed.
# 
# To Test:
#
#   cachedMatrix <- makeCacheMatrix(matrix(sample(100,16,T),4));  
#   cacheSolve(cachedMatrix)
#
# Correct result is the identity matrix with ones on diagonal and 0s (with double precision) elsewhere
#   
#   cachedMatrix$get() %*% cachedMatrix$getinverse()
#
# Caching will only recompute the inverse of the matrix if necessary.
#
# cacheSolve(cachedMatrix)
#   (getting cached data)
#

## makeCacheMatrix assigns forms a data structure around the matrix.
##
makeCacheMatrix <- function(A = matrix()) {
  Ainv <- NULL
  set <- function(y) {
    A <<- y
    Ainv <<- NULL
  }
  get <- function() A
  setinverse <- function(solve) Ainv <<- solve
  getinverse <- function() Ainv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve takes a matrix data structure from makeCacheMatrix the  as a parameter and will
## compute the inverse of that matrix if it does not already exist.
##
## The matrix A is assumed non-singular.  No eigenvalues should be less than a double-precision 0.
##   IE: min(abs(eigen(cachedMatrix$get())$values)) > 1e-14
##
cacheSolve <- function(A, ...) {
  Ainv <- A$getinverse()
  if(!is.null(Ainv)) {
    message("getting cached data")
    return(Ainv)
  }
  data <- A$get()
  Ainv <- solve(data, ...)
  A$setinverse(Ainv)
          ## Return a matrix that is the inverse of 'x'
  Ainv

}
