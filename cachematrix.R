## This function creates a special "matrix" object that can cache its inverse
 
 makeCacheMatrix <- function(x = matrix()) {
         
         m <- NULL
         # Set matrix elements.
         set <- function(a) {
                x <<- a     
                m <<- NULL  
         }
 
         # Function to get matrix data
         get <- function() x
         # Function to set the inverse
         setInverse <- function(inverse) m 
         # Function to get the inverse
         getInverse <- function() m
       
         # Return a list with the above four functions
         list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)        
          
  }

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cacheSolve()
## should retrieve the inverse from the cache.
 
 cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()  
        ##Check if data has been cached
                if(!is.null(m)) {  
                   message("getting cached data")
                   return(m)
               }
         # Data was not cached. 
                 data <- x$get()   
                 m <- solve(data, ...)  
                 x$setInverse(m)   
                 return(m)    
                 }
##Test data
##test <- makeCacheMatrix()
##test$set(matrix(17:20, 2))
##test$get()
##
##     [,1] [,2]
##[1,]   17   19
##[2,]   18   20
##
##cacheSolve(test)			
##
##     [,1] [,2]
##[1,]  -10  9.5
##[2,]    9 -8.5