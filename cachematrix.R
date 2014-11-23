## These functions here return the inverse of matrix
## The key is in caching the inverse, thereby saving time 
## Thus given a matrix if the inverse was computed earlier the inverse will be
## returned from cache

## This function returns a list which has four functions 
## Set Marix, Get Matrix, 

makeCacheMatrix <- function(x = matrix()) {
  MatrixInv <- NULL
  ## This will set the Matrix, 
  ## Since a new matrix is set MatrixInv is set to Null 
  set <- function(InMatrix){
    x <<- InMatrix
    MatrixInv <<- NULL
  }
  ## This will get the matrix set earlier
  get <- function() x
  
  ## This will compute the inverse of the matrix using solve()
  SetInvMatrix <- function(solve) MatrixInv <<- solve
  
  ## This will get the inverse computed by SetInvMatrix
  GetInvMatrix <- function() MatrixInv
  
  ## This will create a list of all the preceding four functions that will
  ## used by the subsequent CacheSolve Function
  list (set=set, get=get,
       SetInvMatrix=SetInvMatrix,
       GetInvMatrix=GetInvMatrix)
}


## This function will return the inverse of the matrix
## The inverse is cached so it is computed only the first time

cacheSolve <- function(x, ...) {
  
  ## Return the Matrix Inverse value
  MatrixInv <- x$GetInvMatrix()
  
  ## If the MatrixInv is already computed then return that
  if(!is.null(MatrixInv)){
    message("getting cached data")
    return(MatrixInv)
  }
 
  ## Compute the Matrix Inverse
  Inmatrix <- x$get()
  MatrixInv <- solve(Inmatrix, ...)
  x$SetInvMatrix(MatrixInv)
  MatrixInv
  
}
  
        
