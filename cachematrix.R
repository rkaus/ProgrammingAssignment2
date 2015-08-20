## makes a makeCacheMatrix object that can return a cached value of it's inverse
## modified version of rdpend example makeVector.R from coursera class 

## creats matrix object
## returns list of methods within this object
## methods include resetting value of the matrix, getting the value of the existing matrix
##      setting the inverse value of the matrix, and getting the inverse value of the existing matrix
## 
## TO DO:
##      1. check if passed matrix is invertible
##      2. 

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
      x <<- y
      i <<- NULL
  }
  
  get <- function() x
  setinverse <- function(inv) i <<- inv
  getinverse <- function() i
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
  
}


## checks if cached inverse exists and alerts user if cached result is returned
## computes inverse and calls on makeCacheMatrix to set inverse value

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)){
    message("getting cached data")
    return(i)
  }
  original <- x$get()
  inv <- solve(original)
  x$setinverse(inv)
  inv
}
