## makeCacheMatrix function takes a matrix and sets it as a Cached Matrix object
makeCacheMatrix <- function(x = matrix()) {
  ## initialize 'm' variable as the Inverse Matrix Cache with NULL value
  m <- NULL  
  
  ## setmatrix() function definition
  setmatrix <- function(y) {
    ## call to setmatrix replaces 'x' variable Matrix Cached Value with the passed matrix
    ## searches 'x' in the parent environment and redefines the value
    x <<- y
    ## call to setmatrix replaces 'm' variable Inverse Matrix Cached value to NULL
    ## searches 'm' in the parent environment and redefines the value to NULL
    m <<- NULL
  }
  
  ## returns 'x' Cached Matrix Value
  getmatrix <- function() x
  
  ## sets 'm' Cached Inverse Matrix Value with the passed solved inverse matrix value from cacheSolve
  setinvmatrix <- function(solvedinvmtx) m <<- solvedinvmtx
  
  ## returns 'm' Cached Inverse Matrix Value
  getinvmatrix <- function() m
  
  ##if no arguments are passed, show list of Cache Matrix functions
  list(setmatrix = setmatrix, getmatrix = getmatrix,
       setinvmatrix = setinvmatrix,
       getinvmatrix = getinvmatrix)
}

## cachSolve function takes a Cached Matrix Object and returns the Inverse of the Cached Matrix
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinvmatrix()
  
  ## if 'm' is not NULL, exit and return the cached inverse matrix retrieved from 'x'
  if(!is.null(m)) {
    message("getting cached inverse matrix data")
    return(m)
  }
  
  ## if 'm' is NULL, retrieve the cached matrix in 'x' to 'data'
  data <- x$getmatrix()
  ## use solve() to get the inverse matrix of 'data' and assign to 'm'
  m <- solve(data, ...)
  ## set the inverse matrix of 'x' with the 'm' solved value using setinvmatrix()
  x$setinvmatrix(m)
  ## return 'a' value - inverse matrix
  m
}