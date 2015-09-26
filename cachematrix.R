## The makeCacheMatrix function createes a list of four functions to set and get a matrix and its inverse.
## The cacheSolve function checks if the inverse was already computed. If yes, it retrieves it. Otherwise
## it computes the inverse and sets the inverse to cache.
makeCacheMatrix <- function(x = matrix()) {
       m <- NULL
       set <- function(y) {
       x <<- y
       m <<- NULL
      }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
              setinverse = setinverse,
               getinverse = getinverse)
}



cacheSolve <- function(x, ...) {
        ## First checks if the inverse was already computed.
        ## Return a matrix that is the inverse of 'x'
        ## Sets the inverse in the cache.
        m <- x$getinverse()
        if(!is.null(m)) {
          message("Getting cached inverse of the matrix")
          return(m)
        }
        matrix <- x$get()
        m <- solve(matrix)
        x$setinverse(m)
        m
  
}
