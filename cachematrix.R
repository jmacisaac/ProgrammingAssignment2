## Caching the Inverse of a Matrix
##  Programming Assignment 2: Lexical Scoping 

## creates a special matrix object, which is a list of functions used to helps store/cache the inverse

makeCacheMatrix <- function(x = matrix()) {
                i <- NULL
                set <- function(y) 
                        {
                                x <<- y
                                i <<- NULL
                        }
                get <- function() x
                
                setInverse <- function(inverse) i <<- inverse
                getInverse <- function() i
                
                list(set = set,
                     get = get,
                     setInverse = setInverse,
                     getInverse = getInverse)
                
                                           }


## computes the inverse 

cacheSolve <- function(x, ...) {
                i <- x$getInverse()
                if(!is.null(i)) 
                {
                message("getting cached data")
                return(i)
                }
                
                data <- x$get()
                
                i <- solve(data, ...)
        
                x$setInverse(i)
                
                i
                                }
