## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL     ## initialize inv as NULL 
  set <- function(y) {    
    x <<- y              
    inv <<- NULL          
  }
  get <- function() x     ## define the get fucntion - returns 
  setInverse <- function(inverse) inv <<- inverse   ## assigns value of inv in parent environment
  
  getInverse <- function() inv                ## gets the value of inv where called
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)


}


## Write a short comment describing this function
## this is used to get cache data

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)) {   ##to check whether inverse is NULL 
    message("get cache data.")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)  ##to calculate invarse 
  x$setInverse(inv)
  inv  ##this is used to provide matrix inverse to X
}
