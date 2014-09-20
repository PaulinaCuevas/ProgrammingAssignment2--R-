#Here I wrote a pair of functions that cache the inverse of a matrix.


#This function creates a special matrix object that can cache its inverse

makeCacheMatrix<-function(m=matrix()){
  inv<-NULL
  set <- function(n) {
    m <<- n
    inv <<- NULL
  }
  get <- function() m
  setinv <- function(solve) inv <<- solve
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


#This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
#If the inverse has already been calculated, then the cachesolve should retrieve the inverse from the cache.

cachesolve <- function(m, ...) {    
  inv <- m$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- m$get()
  inv <- solve(data, ...)
  m$setinv(inv)
  inv
}