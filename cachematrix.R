## These functions create a matrix that can cache its inverse and retrieve it.
## 'makeCacheMatrix()', pretty much like 'makeVector()', assigns to 'x' in another
## environment the value of the matrix (which is the argument for the function)
## and to 'inv', also in another environment, the value of its inverse. It returns
## a list with functions that give access or create such values, which can be used
## through 'cacheSolve()'.

makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  set.matrix <- function(y){
    x <<- y
    inv <<- NULL
  }
  get.matrix <- function() x
  set.inv <- function(solve) inv<<-solve
  get.inv <- function() inv
  list(set.matrix = set.matrix, get.matrix = get.matrix, set.inv = set.inv, get.inv = get.inv)
}


## cacheSolve needs as argument the value of 'makeCacheMatrix()' (i.e. not any matrix)
## and assigns to its own 'inv' that makeCacheMatrix's 'inv'. If such value is
## null (i.e., it was not computed before), it computes it, caches it and
## returns it. If it was already cached, it returns it along with the message
## "getting cached data".

cacheSolve <- function(x, ...) {
  inv <- x$get.inv()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  mat <- x$get.matrix()
  inv <- solve(mat)
  x$set.inv(inv)
  inv     
   ## Return a matrix that is the inverse of 'x', being 'x' a matrix
  ## resulting from 'makeCacheMatrix()'
}
