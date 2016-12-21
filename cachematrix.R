## makecachematrix creates an object that can cache its inverse
##

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- null
  }
  get <- function()x
  
  setmatrix <- function(solveMatrix) inv <<- solveMatrix
  getmatrix <- function() inv
  
  list(set = set, 
       get = get,
      setmatrix = setmatrix,
      getmatrix = getmatrix)
}


## cacheSolve either returns the cached inverse of the matrix above, or calculates it if needed.

cacheSolve <- function(x, ...) {
  
  inv <- x$getmatrix()
  if(!is.null(inv)){
    message("retrieving cached data")
    return(inv)
  }
  
  data <- x$get()
  inv <- solve(data)
  x$setmatrix(inv)
  inv      
}
