## This set of functions is intended to reduce the time of computation of
## scripts that compute a matrix inversion in cases
## where the inversion of the same matrix is used more than once
##
## List of functions:
## * makeCacheMatrix(matrix) - makes an empty cached matrix object
##   inputs: matrix - ordinary R matrix
##   returns: matrix object that contains original matrix and later can contain
##            it's cached matrix inverse
##   example of usage:
##      m <- matrix(c(1,2,3,4), ncol=2, nrow=2)
##      cachedm <- makeCacheMatrix(m)
##      cachedm$get() ## get's the original matrix
##
## * cacheSolve(cached_matrix) - returns an inverse of a cached matrix
##    if run the first time computes an inverse of matrix if it's invertible
##    and returns the computet matrix inverse. If it was already run more
##    than once the last computed inverse is immediately returned
##   inputs: cached_matrix - a matrix object returned by makeCacheMatrix function
##   returns: inverse - when chached_matrix contains invertible matrix
##   example of usage:
##      cminverse <- cacheSolve(cachedm)
##

## makeCacheMatrix - makes a cached matrix object, stores matrix x in
## argument but doesn't compute or contain it's matrix inverse yet
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL    ## original matrix
    minv <- NULL ## cached matrix inverse 
    
    ## sets the matrix, where x is the matrix to be stored
    set <- function(x=matrix())
    {
      m <<- x
      minv <<- NULL ## remove the old matrix inverse
    }
  
    ## returns the stored matrix
    get <- function() m

    ## sets new matrix inverse, where i is the matrix inverse
    setinv <- function(i)
    {
      minv <<- i
    }
    
    ## returns the stored matrix inverse
    getinv <- function() minv
    
    ## important, set stored matrix m to x argument
    set(x)
    
    list(set=set,
         get=get,
         setinv=setinv,
         getinv=getinv)
}


## Return a matrix that is the inverse of 'x', if the inverse isn't computed yet
## then computes it
cacheSolve <- function(x, ...) {
  
  if(is.null(x$getinv()))
  {
    print("i was here")
    tmp <- x$get() ## get the stored matrix
    x$setinv(solve(tmp)) ## compute the matrix inverse and store it into a
                         ## matrix object
  }
  x$getinv() ## return the stored(cached) matrix inverse
}
