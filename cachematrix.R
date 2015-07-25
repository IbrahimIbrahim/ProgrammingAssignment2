makeCacheMatrix <- function(x = matrix()) {
  i  <- NULL
  set  <- function(y){
    x <<- y
    i <<- NULL 
  }
  get  <- function() x
  setinverse  <- function(inverse) i  <<- inverse
  getinverse  <- function() i
  list(set= set, get = get, 
       setinverse = setinverse, 
       getinverse = getinverse)
  
}
cacheSolve <- function(x, ...) {
  
  i  <- x$getinverse()
  message("-----------------------------")
  message("The already saved value = ..")
  print(i)
  message("-----------------------------")
  if (!is.null(i)){
    message("getting cached data")
    return(i)
  }
  message("-----------------------------")
  message("solving ..")
  data  <- x$get()
  i  <- solve(data, ...)
  
  x$setinverse(i)
  i
}