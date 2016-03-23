## The first function makeCacheMatrix takes a square matrix (which should have inverse matrix)
## and creates special 'matrix', saving it to the cache.
## 
## The second function cacheSolve calculates the inverse to the matrix inputed to makeCacheMatrix
## or returns it from cache if it was calculated before.

## The function makeCacheMatrix creates a special "matrix" (of List type)
## which containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse matrix
## 4. get the value of the inverse matrix

makeCacheMatrix <- function(x) {
  M <- NULL
  set <- function(y) {
    x <<- y
    M <<- NULL
  }
  get <- function() x
  setMatrix <- function(solve) M <<- solve
  getMatrix <- function() M
  list(set = set, get = get,
       setMatrix = setMatrix,
       getMatrix = getMatrix)
}


## The function cacheSolve returns the inverse matrix 
## of the special "matrix created with the function makeCacheMatrix.
## It checks if the inverse was already been calculated (for defined matrix)
## If the inverse were calculated before, it gets the inverse matrix from 
## the cache and skips calculation, returning also the string 'getting cached data'
## Otherwise it calculates the inverse matrix and returns it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  M <- x$getMatrix()
  if(!is.null(M)) {
    message("getting cached data")
    return(M)
  }
  data <- x$get()
  M <- solve(data, ...)
  x$setMatrix(M)
  M
}
