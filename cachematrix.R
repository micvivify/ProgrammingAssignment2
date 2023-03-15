# Matrix inversion can be a costly computation. 
# So we can cache the result to avoid computing it repeatedly.
# This assignment is to write a pair of functions that cache teh inverse of a matrix

#function that creates special matrix that can cache its inverse:
makecachematrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


#to be used after above function...returns inverse of the special matrix above.
#if inverse already calculated, then cacheinverse() will get inverse from cache.
cacheinverse <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- inverse(data, ...)
  x$setinverse(i)
  i
}
