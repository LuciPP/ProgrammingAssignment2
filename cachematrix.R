## Assignment 2

##makeCacheMatrix function creates matrix object that can cache its inverse.
makeCacheMatrix <- function(x=matrix()){
  library(MASS)
  inv <-NULL
  set <-function(y){
    x<<-y
    inv<<-NULL
  }
  get <-function() x
  setinverse <- function(ginv) inv<<-ginv
  getinverse <-function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

##cacheSolve function computes the inverse of the matrix returned by makeCacheMatrix above.
##If the inverse has already been calculated (and the matrix has not changed), 
##               then cacheSolve should retrieve the inverse from the cache.
cacheSolve <- function(x,...){
  inv <- x$getinverse()
  if (!is.null(inv)){
    message("Getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- ginv(data,...)
  x$setinverse(inv)
  inv
}
