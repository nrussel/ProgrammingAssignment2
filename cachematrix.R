## Programming Assignment 2 Submission:
## N. Russel

##Function that creates object of type 'Make cache matrix'
##returns list containing functions to interact with 'cachesolve function'

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y) {
            x <<- y
            inv <<- NULL
      }
      get <- function() x
      setinv <- function(solve) inv <<- solve
      getinv <- function() inv
      list(set = set,
           get = get,
           setinv = setinv,
           getinv = getinv)
}


##Function that reads in object of type 'Make Cache Matrix', 
##If matrix inverse has already been calculated/cached, returns cached value
##Otherwise, calculates and returns inverse

cacheSolve <- function(x, ...) {
       ## Return a matrix that is the inverse of 'x'
      inv <- x$getinv()
      if(!is.null(inv)) {
            message("retrieving cached inverse")
            return(inv)
      }
      data <- x$get()
      inv <- solve(data,...)
      x$setinv(inv)
      inv
}
