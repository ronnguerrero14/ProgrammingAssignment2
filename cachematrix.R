## Put comments here that give an overall description of what your
## functions do
##makeCacheMatrix creates creates a special "matrix" object that 
##can cache its inverse
##cacheSolve computes the inverse of the special
##"matrix" returned by makeCacheMatrix above. If the inverse has 
##already been calculated (and the matrix has not changed), then 
##the cachesolve should retrieve the inverse from the cache.


## Write a short comment describing this function
##1.makeCacheMatrix: This function creates a special "matrix" 
##object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  #define setter
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  #define getter
  get <- function() {
    x
  }
  #defines setter for inverse
  setInverse <- function(solve) {
    m <<- solve
  }
  #defines getter for inverse
  getInverse <- function() {
    m
  }
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
 }


##Write a short comment describing this function
##The function cacheSolve returns the inverse of the matrix
##created in the function makeCacheMatrix
##if the matrix cache inverse is available, the cacheSolve
##returns it. Otherwise, it will recalculate and return the
##new cache inverse 
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
    #return message if m is not NULL
    if(!is.null(m)){
      message("getting cached data")
    }
    data<-x$get()
    m<-solve(data, ...)
    x$setInverse(m)
    m
}

  
