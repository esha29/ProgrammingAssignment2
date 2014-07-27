## makeCacheMatrix function creates a special "matrix" object that can cache its inverse
## cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix function. 
## If the inverse has already been calculated (and the matrix has not changed), then the cacheSolve function will 
## retrieve the inverse from the cache.

## makeCacheMatrix function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,            ## return value of the makeCacheMatrix function is a list of functions
       setinverse = setinverse,
       getinverse = getinverse)
}

## cacheSolve function computes, caches, and returns matrix inverse

cacheSolve <- function(x, ...) {       ## return a matrix that is the inverse of 'x'        
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)                          ## return the cached inverse
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m                                   
}
