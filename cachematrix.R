## Functions allow storing in cache matrix and its inverse

## This function creates an objct containing matrix and its inverse with interface
## set sets a matrix
## get returns it
## setinverse "sets" an invers
## getinverse returns it

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) i<<-inv
  getinverse <- function() i
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## This function returns an inverse for x from cache 
## or calculates it and sets it if inverse wasn't stored

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)){
          message("getting cached data")
          return(i)
        }
        X <- x$get()
        i <- solve(X)
        x$setinverse(i)
        i
}
