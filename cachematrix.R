## Put comments here that give an overall description of what your
## functions do

## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather 
## than compute it repeatedly (there are also alternatives to matrix inversion that we will not discuss here). 
## A pair of functions is used to cache the inverse of a matrix.

## Write a short comment describing this function
##Function creates a structure for caching inverted matrix.
##set the value of the matrix
##get the value of the matrix
##set the value of the inverted matrix
##get the value of the inverted matrix

makeCacheMatrix <- function(x = matrix()) {
  
  s <-NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  } 
  get <- function() x
  setsolved <- function(solved) s <<- solved
  getsolved <- function() s
  list(set = set, get = get,
       setsolved = setsolved,
       getsolved = getsolved) 
}

## Write a short comment describing this function
## The function returns inverse matrix. If the result is already stored in cache it serves it. 
## Otherwise it creates inverted matrix using solve function. 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  s <- x$getsolved()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data)
  x$setsolved(s)
  s
}
