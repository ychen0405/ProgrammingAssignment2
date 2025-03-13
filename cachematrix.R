## Below are two functions that are used to create a special object that stores
## a matrix and caches its inverse (assume the matrix is invertible)

## Function makeCacheMatrix creates a special matrix object which is able to cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  get.inverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## Function cacheSolve retrieves cached inverse

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  
  if (!is.null(inv)){
    message("getting cached inverse")
    return(inv)
  }
  
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setinverse(inv)
  inv
        ## Return a matrix that is the inverse of 'x'
}
