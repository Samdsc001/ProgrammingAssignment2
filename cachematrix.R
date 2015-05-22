##  The below functons will Cache the Inverse of a Matrix


#Programming assignment
# makeCacheMatrix: This function creates a 
# special "matrix" object that can cache its inverse.
makeCacheMatrix<- function(m <- matrix())
{
  inverse <- NULL
  set <-  function(y)
  {
    m <<- y;
    inv<<- NULL;  
    
  }
  get <- function() m
  setInverse <- function(inv) inverse <<- inv
  getInverse <- function() inverse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
  
}


# cacheSolve: This function computes the inverse of the special "matrix" 
# returned by makeCacheMatrix function. If the inverse has already been 
# calculated (and the matrix has not changed), then 
# cachesolve retrieves the inverse from the cache

cacheSolve <-  function(m, ...) 
  {
  inverse <- m$getInverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- m$get()
  inverse <- solve(data, ...)
  m$setinverse(inverse)
  inverse

}
