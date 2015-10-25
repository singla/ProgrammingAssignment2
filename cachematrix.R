# Calculates Inverse of matrix and cache it. 

## Returns a list containing functions to - Set/Get Value of matrix;Set/Get value of inverse of matrix  
makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  
  #Function to set value of matrix
  setmatrix<-function(mat)
  {
    x<<-mat
    inv<<-NULL
  }
  
  #Function to get value of matrix
  getmatrix <- function() x
  
  #Function to set inverse of matrix
  setinverse <- function(inverse) inv <<- inverse
  
  #Function to get inverse of matrix
  getinverse <- function() inv
  
  #A list of all above functions
  list(setmatrix = setmatrix, getmatrix = getmatrix,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## Find inverse of matrix created with above function - either from cache(if already computed) or by computing

cacheSolve <- function(x, ...) {
  inv<-x$getinverse()
  if(!is.null(inv))
  {
    message("Getting Cached Data!!")
    return(inv)
  }
  
  #Computing inverse
  mat<-x$getmatrix()
  inv<-solve(mat)
  #Caching inverse computed
  x$setinverse(inv)
  inv
}
