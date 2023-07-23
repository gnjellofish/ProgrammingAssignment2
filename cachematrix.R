## Put comments here that give an overall description of what your
## functions do

## this function creates an input that is placed into cacheSolve()

makeCacheMatrix<- function(x = matrix()) {
  inv <- NULL
  set <- function(y) { ## setting the matrix
    x <<- y ## using <<- to assign value to an object in an environment different from the current
    inv <<- NULL
  }
  get <- function() x
  setinverse<- function(inverse)inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse
  )
}


## this function is the output of makeCacheMatrix() and returns the inverse of the matrix inputted into makeCacheMatrix()

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data") ## if inverse is already calculated then it is grabbed from the cache instead of computing it again
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...) ## calculates inverse otherwise
  x$setinverse(inv) ## sets value into the cache
  inv
}
