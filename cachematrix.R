## Robert A Mooney

## Modeled from Programming Assignment 2 makeVector()

## makeCacheMatrix creates a cached matrix and provides set, get
## and inverse functions

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function (y) {
      x <<- y
      m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set=set, get=get,
       setinverse=setinverse,
       getinverse=getinverse)
  
}



## cacheSolve returns the inverse of the matrix defined by
## makeCachematrix
## Assumes matrix is always a square invertible matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m

}
