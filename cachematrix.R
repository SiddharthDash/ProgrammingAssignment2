##Written by Siddharth Dash<sid.dash01@gmail.com>

#makeCacheMatrix----
#This function creates a special matrix object that can cache it's inverse

#It also contains other functions:
#1. create_matrix : creates a matrix and sets it value
#2. get_matrix : get the value of the matrix
#3. setInverse : sets the value of the inverse
#4. cacheInverse : returns the cached value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  
  x_inverse <- NULL
  create_matrix <- function(y = matrix()){
    x <<- y
    x_inverse <- NULL
  }

  get_matrix <-  function() x
  setInverse <- function(inverse) {x_inverse <- inverse} 
  cacheInverse <- function(){ return(x_inverse)}
  list(set = create_matrix, get = get_matrix , setInverse = setInverse, getInverse = cacheInverse)
  
}

#cacheSolve----
#This function computes the inverse of the special matrix
#If the inverse has already been calculated and the matrix is unchanged, then cacheSolve retrieves the
#inverse from the cache
#
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  x_inverse <- x$getInverse()
  
  if(!is.null(x_inverse))
  {
    message("Inverse of the matrix is cached")
    return(x_inverse)
  }
  m <- x$get()
  x_inverse <- solve(m)
  x$setInverse(x_inverse)
  return(x_inverse)  
  
}
