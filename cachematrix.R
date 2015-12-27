#The makeCacheMatrix function creates a matrix object that can cache its value for future use.
#The cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#If the inverse has already been calculated (and the matrix has not changed), then the cachesolve will 
#retrieve the inverse from the cache.

#The following function sets and gets the matrix values and its inverse

makeCacheMatrix <- function(x = matrix()) {
        cache_inverse <- NULL
	 
	#Set the value of matrix	
        set <- function(y) {
                x <<- y
                cache_inverse <<- NULL
        }

	#Retrieve the matrix values
        get <- function() x

	#Set the inverse of matrix in cache, for future use
        setinverse <- function(inverse) cache_inverse <<- inverse
        
        #Get the inverse of matrix from cache
        getinverse <- function() cache_inverse

        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}



#The following function calculates inverse of a matrix, if not already available in cache

cacheSolve <- function(x, ...) {
        #The following lines get the inverse of matrix from cache
        inverse <- x$getinverse()
        
	#Following if block will return the cached inverse if it's available, and the lines written below the if block will not be executed
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
	#If the inverse is getting calculated for the first time, following lines will get executed
        data <- x$get()
        inverse<- solve(data, ...)
        x$setinverse(inverse)
        inverse

}
