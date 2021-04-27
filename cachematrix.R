##Here we are creating a new matrix "makeCacheMatrix" that will be used to 
##create a new matrix and then return the inverse of the matrix in later.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function( matrix ) {
    m <<- matrix
    i <<- NULL
  }
  get <- function() {
    m
  }
  setInverse <- function(inverse) {
    i <<- inverse
  }
  getInverse <- function() {
    i
  }
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## We need to compute the inverse of the above matrix "makeCacheMatrix". 
## the CacheSolve function will return the inverse of the original matrix if
## the inverse has already been calculated and remains unchanged.

cacheSolve <- function(x, ...) {
  m <- x$getInverse()
  if( !is.null(m) ) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data) %*% data
  x$setInverse(m)
  m
}
