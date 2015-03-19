## First I create a special matrix object using the makeCacheMatrix() function
## then get the inverse of the matrix said using the cacheSolve() function 
## but if the special matrix inverse has already been calculated, the function will 
## find it in the cache and return it, without calculate. 

## This function creates a special matrix object

makeCacheMatrix <- function(x = matrix()) {
  cachedinv <- NULL
  set <- function(y) {
    x <<- y
    cachedinv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) cachedinv <<- inverse
  getinv <- function() cachedinv
  list(set = set, get = get,
       setinv  = setinv,
       getinv = getinv)
}


## This function returns the inverse of the special matrix

cacheSolve <- function(x, ...) {
  data <- x$getreverse()
  if (!is.null(data)) {
    return(data)
  } 
  else {
    data <- solve(x$get())
    x$setreverse(data)
    return(data)
  }
}
