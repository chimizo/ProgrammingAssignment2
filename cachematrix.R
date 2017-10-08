

makeCacheMatrix <- function(x = matrix()) {

  inversa <- NULL
  set <- function(y) {
    x <<- y
    inversa <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inversa <<- inverse
  getInverse <- function() inversa
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}



cacheSolve <- function(x, ...) {
  
  inversa <- x$getInverse()
  if (!is.null(inversa)) {
    message("getting cached data")
    return(inversa)
  }
  mat <- x$get()
  inversa <- solve(mat, ...)
  x$setInverse(inversa)
  inversa
}
