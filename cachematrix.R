## Put comments here that give an overall description of what your
## functions do

## Function to cache the inverse of input matrix x
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInv <- function(m_inv) inv <<- m_inv
  getInv <- function() inv
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


## Find the inverse of the input matrix x. 
## If it is not found in cache, find the inverse for the first time & return it.
## Else return it from the cache itself.
## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
  inv <- x$getInv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setInv(inv)
  inv
}
