
## Matrix inversion is a costly computation.  It will be benefit to cache the inverse
## of a matrix rather than compute it repeatedly.  This pair of function will  
## calculate the inverse of a matrix and cache its result.  In order to cache the result,
## a special "matrix" object will be created

## This function creates a special "matrix" object that can cache its inverse.  This 
## function will retrieve or store the inverse of matrix in cache.  
## This function will provide a list of following operations.
## setmatrix() - set the value of matrix
## getmatrix() - get the value of matrix
## setinverse() - set the inverse of matrix
## getinverse() - get the inverse of matrix

makeCacheMatrix <- function(x = matrix()) {

  ## initialize there is no cache inverse matrix
  inverseMatrix <- NULL
  
  #set the matrix and indicate the inverse matrix is not set
  setmatrix <- function(y) {
    matrix1 <<- y
    inverseMatrix <<- NULL
  }
  
  ## get the matrix
  getmatrix <- function() matrix1
  
  ## Set the inverse of matrix in cache
  setinverse <- function(matrix2) inverseMatrix <<- matrix2
  
  ## Get the inverse of matrix from cache
  getinverse <- function() inverseMatrix
  
  matrix (setmatrix = setmatrix, getmatrix = getmatrix,
          setinverse = setinverse, getinverse = getinverse)
}

## This function computes the inverse of the special "matrix" object returned by makeCacheMatrix() 
## above.  If the inverse has already been calculated and the matrix has not changed, 
## then the cachesolve() will retrieve the inverse from the cache.  If the inverse has not been
## calculated, this function will calculate the inverse of the matrix and cache the result

cacheSolve <- function(x, ...) {

## Check to see if there is an inverse matrix already cache
  
  matrix2 <- x$getinverse()
  if(!is.null(matrix2)) {
    message("getting cached data for inverse matrix")
    return(matrix2)
  }
## There is no cache copy, calculate an inverse matrix  
  data <- x$getmatrix ()

## Calculate an inverse of matrix and save it in cache
  matrix3 <- solve(data, ...)
  x$setinverse(matrix3)

## Return a matrix that is the inverse of 'x'
  matrix3
  
}

