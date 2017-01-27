## Hi! 
## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

## makeCacheMatrix is a function that creates an invertible matrix and is really a list containing a function to:
##1: set the matrix
##2: get the matrix
##3: set the inverse of the matrix
##4: get the inverse of the matrix



makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y){
    x <<- y
    inverse <<- NULL
  }
  get <- function()x
  set_inverse <- function(inverse)
    inverse <<- inverse
  get_inverse <- function()inverse
  list(set = set, get = get, 
       set_inverse = set_inverse,
       get_inverse = get_inverse)
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. It first checks to see if the inverse has already been calculated, if so it gets the inverse from the cache and skips the computation.

cacheSolve <- function(x,...){
  #We want to get the inverse of the matrix already defined in the scope makeCacheMatrix
  inverse <- x$get_inverse()
  if (!is.null(inverse)){
    message("getting cached data")
    return(inverse)
    #The first time cacheSolve is called the value of the inverse is NULL therefore the function will skip the if condition and calculate the inverse before setting its values in the cache
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$set_inverse(inverse)
  
  return(inverse)
  
} 
##Thank you for your help