## 
##    Filename: cachematrix.R
## Description: contains functions used to create a special matrix object that will store in
##              its cache the solve inverse of itself
##   Functions: makeCacheMatrix
##              cacheSolve

##
##    Function: makeCacheMatrix
##  Parameters: x - special matrix object, ... parameters 
## Description: creates a special matrix object used for caching its inverse after calculation
##
makeCacheMatrix <- function(x = matrix()) {
  # initialize cached inversed matrix to NULL
  InverseMatrix <- NULL
  
  # set function - initializes the internal matrix
  set <- function(y) {
    # attribute y to x (our internal matrix)
    x <<- y
    
    # clear the cached inverse of the matrix (if existant)
    InverseMatrix <<- NULL
  }
  
  # get function - returns the internal matrix
  get <- function() x
  
  # setinverse function - store the inverse of the matrix to cache
  setinverse <- function(inverse) InverseMatrix <<- inverse
  
  # setinverse function - read the inverse of the matrix from cache (will be NULL if nonexistant)
  getinverse <- function() InverseMatrix
  
  # return our special matrix object with its functions
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


##
##    Function: cacheSolve
##  Parameters: x - special matrix object, ... parameters 
## Description: Solves the inverse of a matrix
##

cacheSolve <- function(x, ...) {
  # try to get the inversed matrix from cache
  InversedMatrix <- x$getinverse()
  if(!is.null(InversedMatrix)) {
    # if we get it from cache then return value and exit function
    message("getting cached data")
    return(InversedMatrix)
  }
  
  # we couldn't get the inversed matrix from cache, so lets solve it
  # getting matrix
  data <- x$get()
  
  # calculating inversed matrix
  InversedMatrix <- solve(data, ...)
  
  # save inversed matrix in cache
  x$setinverse(InversedMatrix)
  
  # return the inversed matrix we just calculated
  InversedMatrix
}
