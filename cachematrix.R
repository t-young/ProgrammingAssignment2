## cachematrix.R
##
## File contains two functions that are used for computing the
## inverse of a given matrix and caching its results.

## Function creates a list that contains accessor/mutator functions
## for the given matrix and its inverse.
##
## Parameters:
##  x - Matrix to have its inverse cached.
##
## Return:
##  A list that contains accessor/mutator functions for the given
##  matrix and its inverse.
##
makeCacheMatrix <- function( x = matrix() )
{
  inv <- NULL
  
  set <- function( mat )
  {
    x   <<- mat
    inv <<- NULL
  }
  
  get <- function() x
  setInverse <- function( iMat ) inv <<- iMat
  getInverse <- function() inv
  
  list( set = set, get = get, setInverse = setInverse, getInverse = getInverse )
}


## Functions takes a list that was created with the 'makeCacheMatrix'
## function and computes the inverse of the matrix that is an item
## within the given list and caches it within the list. If the inverse
## has already been computed then it will just return the cached value.
##
## Paramter:
##  x - The list that contains the matrix to have its inverse computed.
##
## Return:
##  The inverse of the given matrix.
##
cacheSolve <- function(x, ...)
{
  inv <- x$getInverse()
  if( !is.null( inv ) )
  {
    return ( inv )
  }
  
  mat <- x$get()
  inv <- solve( mat, ... )
  x$setInverse( inv )
  
  inv
}
