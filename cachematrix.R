## This function creates a special "matrix" object that can cache its inverse

#makeCacheMatrix <- function(x = numeric()) {

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse
       )
}  
  

## this function computes the inverse of the special "matrix" returned by makeCacheMatrix above
## It first tries to return the object from the cache, if the cache is empty it computes the 
## inverse

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  
  #if the matrix's inverse has been cached, return the cached value.
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  #the matrix's inverse is not stored in the cache.
  #compute with solve() and save it
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m  
}

