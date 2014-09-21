## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  
  ## declaring inv_mat variable null
  inv_mat <- NULL
  
  ## this set method will set x to y and will store in global env
  set <- function(y){
    print("in set method")
    x <<- y
    inv_mat <<- NULL    
  }
  
  ## this get method will return x 
  get <- function() {
    x
  }
  
  ## this method is to set mat_mean as mean and also puttng in cache
  setinverse <- function(inv) {
    inv_mat <<- inv    
  }
    
  ## This is to return inv_mat
  getinverse <- function(){
    inv_mat
  }
  
  ## this is to make list having set, get, setinverse and getinverse objects 
  list(set = set, get = get, setinverse=setinverse, getinverse= getinverse)    

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  ## this is to get matrix inverse from the x object, which is a list
  inv_mat <- x$getinverse()
  
  ## this will check if inv_mat has a value, if it has then will return cached value
  ## if inv_mat is not null, then catchSolve will return the cached value
  if(!is.null(inv_mat)) {
    message("getting cached data")
    return(inv_mat)
  }
  
  ## if inv_mat is empty(null) then it will proceed further
  ## this is to get the original matrix from x  
  org <- x$get()
  
  ## below solve(org) will return the inverse of the matric and will set as inv_mat
  inv_mat <- solve(org)

  ## this is to set x list setinverse object as inv_mat
  x$setinverse(inv_mat)
  
  ## this wll return inv_mat
  inv_mat
}
