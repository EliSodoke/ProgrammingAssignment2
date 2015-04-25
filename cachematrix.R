## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  
  inverse<-NULL
  
  set<-function(mat)
  {
    x <<- mat
    inverse <<- NULL
  }
  
  get<-function() x;
  setInverse <- function(inverseValue) inverse <<- inverseValue
  getInverse <- function() inverse
  
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  solved <- x$getInverse()
  
  
  if(!is.null(solved)) {
    message("getting cached data")
    return(solved)
  }
  
  Mat <- x$get()
  solved <- solve(Mat)
  x$setInverse(solved)
  solved
  
}

## Test caching
## The value of can be changes to generae bigger matrix
n <- 3
mat <- matrix(rnorm(1:(n*n)), nrow=n, ncol=n)
matCached <- makeCacheMatrix(mat)
matSolved1 <- cacheSolve(matCached)
matSolved2 <- matCached$getInverse()
if (!identical(matSolved1, matSolved2)) message("Cached version does not match solved version")
