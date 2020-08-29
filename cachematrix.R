## The function makeCacheMatrix() returns a list containing a function to
## 1. set the value for a matrix
## 2. get the value for the matrix
## 3. set the value for the inverse
## 4. get the value for the inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  get <- function() x
  
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  
  ##This function outputs a list.
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

##The following function checks if an INVERSE of the matrix generated from
##the function "makeCacheMatrix()" exists, if so, then it returns the 
## INVERSE. If it doesn't exists, it calculates and returns the INVERSE 
## of the matrix. And also sets it to memory for future reference.

cacheMatrix <- function(x, ...) {
  m <- x$getinverse()
  
  ##To check if inverse already exists.
  if(!is.null(m)) {
    message("Obataining cached data")
    return(m)
  }
  
  ##To calculate inverse
  data <- x$get()
  m <- solve(data, ...)
  
  ##To set the value for inverse calculated to memory for future refernce.
  x$setinverse(m)
  
  ##Next Step returns the matrix
  m                           
}
