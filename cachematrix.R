## makeCacheMatrix function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y){
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(Inverse) inverse <<- Inverse
  getinverse <- function() inverse
  list(set = set, get = get, setinverse = setinverse, 
       getinverse = getinverse)
  
} ## end of makeCacheMatrix function


## cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
## If the inverse has not been calculate, it will compute it and return it.

cacheSolve <- function(x, ...) 
{
  inverse <- x$getinverse()
  
  ## if the inverse is already in memory, we will return the cached inverse
  if(!is.null(inverse)){
    message("The inverse of this matrix has already been calculated. Getting cached inverse...")
    return(inverse)
  }
  
  ## inverse is not in the memory, so we will calculate the inverse and return it
  data <- x$get()
  inverse <- solve(data)
  x$setinverse(inverse)
  inverse ## return the inverse of the matrix
  
} ## end of cacheSolve function
