makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  
  set <- function(y)
  {
    x <<- y
    inv <<- NULL
  }
  
  get <- function()x
  
  setinvers <- function(inverse)
  {
    inv <<- inverse
  }
  
  getinvers <- function()inv
  
  list(set=set, get=get, setinvers=setinvers, getinvers=getinvers)
}



cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  
  if(!is.na(inv))
  {
    message("getting cashed Data")
    return(inv)
  }
  
  data <- x$get()
  
  inv <- solove(data,...)
  x$setinvers(inv)
  inv
}
