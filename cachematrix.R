## These functions cache a given matrix inversion to conserve 
## repeating the computationally complex inversion calculation.
## For the given matrix it will only be calculated once.


## makeCacheMatrix provides the functions to get and set the matrix
## as well as get and set the inverse
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


## cacheinverse calculates the matrix inverse if it is not cached
## and reads the cache if it is available intead of calculating
cacheinverse <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}

## test console session
##
# > x <- rbind(c(4,7),c(2,6))
# > a <- makeCacheMatrix(x)
# > cacheinverse(a)
# [,1] [,2]
# [1,]  0.6 -0.7
# [2,] -0.2  0.4
# > cacheinverse(a)
# getting cached data
# [,1] [,2]
# [1,]  0.6 -0.7
# [2,] -0.2  0.4
# >

