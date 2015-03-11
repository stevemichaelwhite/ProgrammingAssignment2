## There are two function in this file: makeCacheMatrix and cacheSolve
## The purpose of these is to allow caching the matrix in order to avoid 
## recomputing an inverse if it has already been calculated.  
## The goal is to save computation time.

## !It is assumed that the original matrix is invertible!


## makeCacheMatrix takes a matrix as an argument and returns a list of functions: set, get, setinverse, getinverse

makeCacheMatrix <- function(x = matrix()) {
  m.inverse <- NULL
  # reset the matrix
  set <- function(y) {
    x <<- y
    m.inverse <<- NULL # inverse goes back to null when we reset the matrix
  }
  get <- function() x
  setinverse <- function(inverse) m.inverse <<- inverse # update m.inverse defined by makeVector
  getinverse <- function() m.inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## cacheSolve accepts a makeCacheMatrix function object and will set the inverse matrix for it.  It also returns the inverse matrix.

cacheSolve <- function(x, ...) {
  # x is an instance of the makeVector function
  # has an inverse already been calculated?
  m.inverse <- x$getinverse()
  if(!is.null(m.inverse)) {
    message("getting cached inverse matrix")
    return(m.inverse)
  }
  # if not, then let's load the matrix
  data <- x$get()
  # And perform the potentially costly inverse operation
  #leave out the b argument so solve will compute inverse of data
  m.inverse <- solve(data, ...)
  # Then store it
  x$setinverse(m.inverse)
  m.inverse
}