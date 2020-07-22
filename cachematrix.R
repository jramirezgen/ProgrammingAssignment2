## Assignment: Caching the Inverse of a Matrix
## makeCacheMatrix,cacheSolve

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  set_Inverse <- function(inverse) m <<- inverse
  get_Inverse <- function() m
  list(set = set, get = get,
       set_Inverse = set_Inverse,
       get_Inverse = get_Inverse)
}


##This function computes the inverse of the special "matrix"
  ##returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$get_Inverse()
  
  if(!is.null(m)){
    
    message("getting cached data")
    
    return(m)
  }
  mat <- x$get()
  
  m <- solve(mat, ...)
  
  x$set_Inverse(m)
  
  m
}

