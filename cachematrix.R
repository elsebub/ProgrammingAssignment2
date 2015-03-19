## I copied pretty much the exsample that Robert had on caching the mean of a vector
## I changed the code to work on matrices
## Use it like this: y <-  makeCacheMatrix(x), where x is an invertible matrix
## then call cacheSolve(x)
## the first time you run it it returns the inverse of the matrix, but the second time it should 
## also print "getting cached data"

## This function creates a special "matrix" object that 
## can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  inverse <- NULL
  
  ## define functions set(), get(), setinv(), getinv()
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  
  get <- function() x
  
  setinv <- function(solvedmatrix) inverse <<- solvedmatrix
  
  getinv <- function() inverse
  
  ## make a list of function you can use for the Matrix
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)

}


## This function computes the inverse of the special "matrix" returned 
##by makeCacheMatrix above. If the inverse has already been calculated 
##(and the matrix has not changed), then the cachesolve should retrieve 
##the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinv()
  
  ##check for a cached matrix
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  
  data <- x$get()
  
  ## calculate the inverse
  inverse <- solve(data, ...)
  
  ## cache the inverse calculated
  x$setinv(inverse)
  
  ## return inverse
  inverse
  
}
