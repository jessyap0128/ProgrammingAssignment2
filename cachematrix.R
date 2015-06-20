## These pair of functions aim to cache the inverse of a matrix 
## rather than compute it repeatedly 

## makeCacheMatrix creates a special "matrix" object that can cache 
##its inverse.

makeCacheMatrix <- function(x = matrix()) 
		{
         	i <- NULL
	 	setMatrix <- function(y) 
			     {
				x <<- y
				i <<- NULL
		     	      }
		
	 	getMatrix <- function() x
	 	setInverse <- function(inverse) i <<- inverse
         	getInverse <- function() i

		list(setMatrix = setMatrix, getMatrix = getMatrix, 
            	setInverse = setInverse, getInverse = getInverse)		
		 }


## CacheSolve computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then cacheSolve should retrieve 
## the inverse from the cache.

cacheSolve <- function(x, ...)									
	     {	
	     ## Return a matrix that is the inverse of 'x'
		i <- x$getInverse()
		if(!is.null(i))
		   {
		    	message("getting cached data")
			return(i)
		   }
		
		data <- x$getMatrix()
		i <- solve(data, ...)
		x$setInverse(i)
		i

	     }

