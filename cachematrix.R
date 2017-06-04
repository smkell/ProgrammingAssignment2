#' Creates a new object representing the cache.
#' 
#' @param x A seed matrix to build the cache from.
#' @return An object which caches the value of a matrix and its inverse.
#' @examples 
#' makeCacheMatrix(matrix(c(1,2,3,4), nrow=2, ncol=2))
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x 
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function(inverse) m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

#' Returns a matrix that is the inverse of 'x'
#' 
#' @param x A caching matrix (created with `makeCacheMatrix`) to solve the inverse for
#' @return The inverse of the matrix represented by `x`
#' @examples
#' cacheSolve(makeCacheMatrix(matrix(c(1,2,3,4), nrow=2, ncol=2)))
cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if (!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
