## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

  ## set the value of the matrix
  
  
  cache <- NULL
  set <- function(y){
    
    x <<- y
    cache <- NULL
    
  }
  
  ## get the value of the matrix
  
  get <- function() x 
  
  ## store inverted matrix
  
  setinv <- function(inv) cache <<- inv
  
  ## print inverted matrix
  
  getinv <- function()cache
  
  ## list of functions
  
  list(set = set , get =get, 
       setinv=setinv,getinv=getinv)
  
 }
  

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data.")
    return(cache)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinv(inv)
  inv
}



## Test the functions

a <-  makeCacheMatrix(matrix(1:4,ncol=2))

# print the matrix
a$get()

# print not yet computed inverse matrix
a$getinv()

### compute the inverse matrix

cacheSolve(a)


## call stored inverse matrix again

a$getinv()








