
##################################################################################
##                         CACHING THE INVERSE OF A MATRIX                      ##
##################################################################################

## Functions to cache the inverse of a matrix instead of computing it repeatedly. 

## TEST example:
# mat <- matrix(1:4,2,2)
# cMat <- makeCacheMatrix(mat)
# cMat$get()                        # returns the same thing as mat.
# cacheSolve(cMat)                  # 1st call: returns the inverse.
# cacheSolve(cMat)                  # 2nd call: returns the cached inverse.

## 1/ makeCacheMatrix: creates a special "matrix" object, which is really a list 
# containing a function to:
# (i)   set the value of the matrix
# (ii)  get the value of the matrix
# (iii) set the value of the inverse
# (iv)  get the value of the inverse

makeCacheMatrix <- function(x = numeric()) {
      inv <- NULL
      set <- function(y) {
            x <<- y
            inv <<- NULL
      }
      get <- function() x      
      setsolve <- function(solve) inv <<- solve
      getsolve <- function() inv
      list(set = set, 
           get = get, 
           setsolve = setsolve, 
           getsolve = getsolve)
}

## 2/ cacheSolve: computes the inverse of the matrix returned by makeCacheMatrix. 
## If the inverse has already been calculated AND the matrix has not changed, 
## then cacheSolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
      inv <- x$getsolve()
      if(!is.null(inv)) {
            message("Getting cached data")
            return(inv)
      }
      data <- x$get()
      inv <- solve(data, ...)
      x$setsolve(inv)
      inv
}
