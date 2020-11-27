##First function creates a special "matrix" object that can cache its inverse.

##Second function computes the inverse of the special "matrix" returned by 
#`makeCacheMatrix` above. If the inverse has already been calculated (and the matrix
#has not changed), then 'cacheSolve` should retrieve the inverse from the cache.

## Write a short comment describing this function
#this makes a list of functions and the cache
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  get <- function() x
  
  setinv <- function(inv) i <<- inv
  getinv <- function() i
  
  list( set=set, get=get, setinv=setinv, getinv=getinv)
}


## Write a short comment describing this function
#This function uses the previous function to get inverse
cacheSolve <- function(x, ...) {
  i <- x$getinv()
  if(!is.null(i)){
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i
        ## Return a matrix that is the inverse of 'x'
}
