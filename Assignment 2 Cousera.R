

makeCacheMatrix <- function(x = numeric()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}

#Testing my functions
x <- makeCacheMatrix(matrix(1:3,2,2))
x$get()
x$getinverse()
cacheSolve(x)
 [,1] [,2]
[1,]    1    3
[2,]    2    1

> x$getinverse()
NULL
> cacheSolve(x)
     [,1] [,2]
[1,] -0.2  0.6
[2,]  0.4 -0.2
> 