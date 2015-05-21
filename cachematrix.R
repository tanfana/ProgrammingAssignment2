 ############################################################################
 ## Sometimes it is usefull to cache your computations because they could 
 ## need a lot of time if you have big data.
 ## The two functions below create a special object (in this case a matrix)
 ## and cache's its inverse.
 ##
 ## We have the parent function makeCacheMatrix() an four inner funcions
 ## set(), get(), setInverse() and getInverse().
 ## The inner functions, also referred to as nested functions, have access 
 ## to the parent functions environment.
 ## This means that the inner function can use for example the variables,
 ## arguments, etc. of the parent function.
 ###########################################################################


 ## In this case my makeCaheMatrix() function creates a matrix which is a list 
 ## containing four functions:

 ## 1. set the value of the matrix
 ## 2. get the value of the matrix
 ## 3. set the value of the inverse
 ## 4. set the value of the inverse
 
 makeCacheMatrix <- function(x = matrix()) {
         
         inv <- NULL            ## inv is a local variable created by makeCacheMatrix()
         set <- function(y){    ## set() is the inner function, a closur
                 x <<- y
                 inv <<- NULL   ## use variable declared in the parent function
         }
         get <- function() x
         setInverse <- function(solve) inv <<- solve
         getInverse <- function() inv
         
         list(set=set, get=get,
              setInverse=setInverse,
              getInverse=getInverse)
 }
 
 
 
 ## The following function cacheSolve() calculates the inverse
 ## of the special "matrix" created with the above function
 ## makeCacheMatrix().
 ## First: It checks to see if the inverse has already 
 ## been calculated. If so, it gets the inverse from the
 ## cache and skips the computation. 
 ## Otherwise, it calculates the inverse of the data and
 ## sets the value of the inverse in the cache via the
 ## setInverse() function.
 
 
 cacheSolve <- function(x, ...) {
         
         inv <- x$getInverse()   ## Return a matrix that is the inverse of 'x'
         if(!is.null(inv)) {     ## checks whether the inverse already exists or not
                 message("getting cached data")
                 return(inv)
         }
         data <- x$get()        
         inv <- solve(data, ...) ## if not, calculates the inverse
         x$setInverse(inv)
         inv
 }
 
 
 ## m<-matrix(c(1,2,2,1),2,2)
 ## mat<-makeCacheMatrix(m)
 ## cacheSolve(mat)
 
