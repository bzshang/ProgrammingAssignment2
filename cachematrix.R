## makeCacheMatrix is a 'matrix' object that caches the matrix inverse.
## cacheSolve returns the matrix inverse, first checking if it has been cached.

#This function returns a list of functions enabling
#the inverse of a matrix 'x' to be cached.
#set() enables the matrix 'x' to be set.
#get() returns the current matrix 'x'.
#setinv() caches the inverse into the variable 'inv'.
#getinv() returns the inverse 'inv'.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


#This function returns a matrix that is the inverse of 'x'.
#It first checks if the inverse has been computed before and 'x' unchanged.
#If so, it returns the cached inverse.
#Otherwise, it computes the inverse and caches it.
cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
