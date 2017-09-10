##These R functions caches matrix inversions, which are time consuming computations.
#These functions caches the inverse of a matrix. 

#This function creates a special "matrix" that can cache its inverse 

makeCacheMatrx <- function(x = matrix()){
  inv <- NULL
  set <-function(y){
    x <<- y 
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(get=get, set=set, getinverse=getinverse, setinverse=setinverse)
}


#This function computes the inverse of the special "matrix" returned by makeCacheMatrix.
#This function checks if the inverse has already been calculated and if so this function retrieves it
#from the cache.


cacheSolve <- function(x,...){
  inv <- x$getinverse()
  if(!is.null(inv)){
    message("getting fetched data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data,...)
  x$setinverse(inv)
  inv
}
