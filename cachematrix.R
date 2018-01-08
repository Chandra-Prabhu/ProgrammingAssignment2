## makeCacheMatrix function creates a special "matrix" object that can cache its inverse
## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix above.
##    If the inverse has already cached, then it retrieve it

## makeCacheMatrix - create, cache matrix & its inverse

makeCacheMatrix <- function(x = matrix()) {
  minverse <- NULL                             ## initialising inverse
  set <- function(y=matrix()) {
    x <<- y                                    ## updating matrix
    minverse <<- NULL
  }
  get <- function() x                          ## export matrix to Cachesolve
  setinverse <- function(inv) minverse <<- inv ## import inverse from Cachesolve
  getinverse <- function() minverse            ## report inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


##cacheSolve - retrieve inverse from cache or calculate & return inverse, also send it to cache

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {                              ## check Cache
    message("getting cached data")
    return(m)                                    ## report inverse 
  }
  data <- x$get()                                ## import matrix
  m <- solve(data, ...)                          ## compute inverse of matrix
  x$setinverse(m)                                ## export inverse to the cache
  m
}
