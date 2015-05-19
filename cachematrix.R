## There are two functions that cache and compute the 
## inverse of matrix.

## This function creates a special matrix object that can 
##cache its inverse.

makeCacheMatrix <- function(mtx = matrix()) {
         inverse <- NULL 
         set <- function(x)
         { 
         mtx <<- x; 
         inverse <<- NULL; 
          }
         get <- function() return(mtx); 
        setinv <- function(inv) inverse <<- inv; 
        getinv <- function() return(inverse); 
         return(list(set = set, get = get, setinv = setinv, getinv = getinv))

}


## The fuction cacheSolve returns the inverse of matrix
## which is returned by makeCacheMatrix.If the inverse has been created 
## already and matrix is not changed,cacheSolve will retrieve inverse from the cache 

cacheSolve <- function(mtx, ...) {
        ## Return a matrix that is the inverse of 'x'
          inverse <- mtx$getinv() 
    if(!is.null(inverse)) { 
        message("Getting cached data...") 
         return(inverse) 
    } 
     data <- mtx$get() 
    inverse <- solve(data, ...) 
     mtx$setinv(inverse) 
     return(inverse) 

}
