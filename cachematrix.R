#this is Programmin Assignment 2 - Lexical Scoping lesson

#This function creates a special "matrix" object that can cache its inverse.

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


#This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.

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
