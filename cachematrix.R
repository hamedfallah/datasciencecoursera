
#in this part I have functions that cache the inverse of matrix.
# I create a matrix object that caches its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(solveMatrix) m <<- solveMatrix
  getInverse <- function() m
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)

}
#here is the function that gives the inverse of mathrix returned by makeCacheMatrix.
# the "if" clause checks if the invers has already been computed or not. 
# the solve function computes the invrse of matrix

cacheSolve <- function(x, ...) {
        
  m <- x$getInverse()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setInverse(m)
  m      
}
