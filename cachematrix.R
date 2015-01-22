## Assignment 2 Stub filled by Bulent CANDAN on 01-22-2015

## The function "makeCacheMatrix" creates a matrix which is cached both the matrix "x" itself
## and the inverse of the matrix "x" named as "inverse". The function has four sub functions 
## to implement the cached matrix "x" and inverse of it "inverse" four sub functions are
## implmented. Explained in implementation order as:
## 1. setx       : cahces the matrix "x", and invalidates its inverse since "x" has changed
## 2. getx       : gets the cahced matrix "x"
## 3. setinverse : the inverse of matrix is cached into "inverse"
## 4. getinverse : the cached "inverse" is returned. It is NULL if "x" is newly created or 
##                 is updated otherwise it is th "inverse" of matrix "x"

makeCacheMatrix <- function(x = matrix()) { ## create matrix "x"
  
  inverse <- NULL    ## When Cached Matrix "x" is created its "inverse" is initialized to NULL
  ## to indicate that the inverse of matrix "x" has not been computed
  
  ## Sub function declarations for "makeCacheMatrix"
  setx      <- function(matrix_x) {  
    x       <<- matrix_x    ## Matrix "x" is updated and cached
    inverse <<- NULL        ## Update of matrix "x" invalidates cached "inverse" matrix of "x"
  }
  
  getx       <- function() x ## Return cached matrix "x"
  
  setinverse <- function(inverseOfx) inverse <<- inverseOfx ## Cache the "inverse" of matrix "x"
  
  getinverse <- function() inverse ## get the cached "inverse" of matrix "x"
  
  ## "makeCacheMatrix" code body starts here
  
  list(setx       = setx,  ## generate the sub command function pointer list of function "makeCacheMatrix"
       getx       = getx,  ## so that sub commands can be called externally    
       setinverse = setinverse,
       getinverse = getinverse)
}


## The function "cacheSolve" make use of the function "makeCacheMatrix" to compute and 
## cache the inverse of matrix "x". It first checks whether the "inverse" has been 
## computed that is "inverse" is not NULL. If not NULL cached "inverse" of matrix "x" is
## returned otherwise the cahced matrix "x" is retrieved and its inverse is computed by the 
## R library function "solve". The output of "solve" is cached into "inverse" and also returned.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of "x" which is created by "makeCacheMatrix"
  
  inverse <- x$getinverse() ## fetch the cached "inverse"
  
  if(!is.null(inverse)) {  
    ## Not NULL thus "inverse" is valid then return it without computing the inverse
    message("getting the cached inverse of the matrix")
    return (inverse) ## exit the function by returning "inverse" cached already
  }
  
  ## "inverse" is NULL ie matrix "x" is either newly created or updated
  matrix_x <- x$getx()               ## get cached matrix "x"
  inverse  <- solve(matrix_x, ...)   ## compute inverse of matrix "x" by solve()
  ## passing the user set parameters to "solve" as well
  x$setinverse(inverse)              ## cache the "inverse" 
  inverse                            ## also return it
}
