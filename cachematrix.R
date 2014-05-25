## Since matrix inversion is computationally expensive, we cache the inverse of matrix
## using the following two functions



## function "makeCacheMatrix" creates a special "matrix" object

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y)  # caches the matrix
  {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  setcache <- function(solve) inv <<- solve
  getcache <- function() inv
  list(set=set, get=get, setcache=setcache, getcache=getcache) #returns a list of functions

}



## function "cachesolve" returns a matrix that is the inverse of the matrix 'x' 

cacheSolve <- function(x, ...) {
  
  inv <- x$getcache()
  if(!is.null(inv)) # to check whether inv is already calculated
  {
    message("getting cached matrix")
    return(inv)
  }
  
  data <- x$get()  # fetches the cached value/matrix
  ## solve() applied to a matrix (square invertible) returns the inverse
  inv <- solve(data, ...) 
  x$setcache(inv)
  inv          
}



