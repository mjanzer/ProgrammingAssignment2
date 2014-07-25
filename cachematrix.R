## Assignment: Caching the Inverse of a Matrix

## Example of how to utilize listed below
## Note, cachceSolve only works on invertible matrices


## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(solve) inv <<- solve
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should 
## retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}



###Example of how to utilize these 2 functions

##solving the traditional way
# xtrad<-rbind(c(1, -1/4), c(-1/4, 1))
# xtrad
# solve(xtrad)

##Solving utilizing these functions
# x<-makeCacheMatrix(rbind(c(1, -1/4), c(-1/4, 1)))
# x$get()
# x$getinv()
# cacheSolve(x)
# x$getinv()
# cacheSolve(x)
## Change the matrix stored 
# x$set(rbind(c(1, 3), c(3, 1)))
# x$get()
# x$getinv()
# cacheSolve(x)      
# x$getinv()
# cacheSolve(x)      

