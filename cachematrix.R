
# Matrix inversion is usually a costly computation and there may be some benefit
# to caching the inverse of a matrix rather than compute it repeatedly.
# The following two functions are used to cache the inverse of a matrix.



# Function 1: makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
  inv_matrix <- NULL
  set <- function(y) {
    x <<- y
    inv_matrix <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv_matrix <<- inverse
  getinverse <- function() inv_matrix
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}



# Function 2: cacheSolve to return the inverse of the matrix. 
# 1. It first checks if the inverse has already been computed. 
# 2. If so, it gets the result and skips the computation. 
# 3. If not, it computes the inverse, sets the value in the cache via setinverse function.
# This function assumes that the matrix is always invertible.
cacheSolve <- function(x, ...) {
  inv_matrix <- x$getinverse()
  if(!is.null(inv_matrix)) {
    message("getting cached data")
    return(inv_matrix)
  }
  data <- x$get()
  inv_matrix <- solve(data)
  x$setinverse(inv_matrix)
  inv_matrix
}

