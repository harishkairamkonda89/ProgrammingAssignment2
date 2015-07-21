
## The following functions calculate the inverse of a matrix and saves it
## to the cache, so that next time when attempted again, the previously 
## saved value is returned instead of repeating the caluculation



## This function creates an object and contains a function to set the value
## and get the value of matrix and the inverse



makeCacheMatrix <- function(x = matrix()) 
{
## create a matrix object x and associated sub-functions
## define the cache m
          m <- NULL
	  set <- function(y) 
	       {
	         
		 x <<- y ##assign input matrix to variable x
		 m <<- NULL ##re-intiallizing in the parent env to null

               }
	  get <- function()x ##return the matrix x
	  setinverse <- function(inverse) m <<- inverse 
	  ##set the cache m equal to the                                                  ##inverse of the matrix x
	  
	  getinverse <- function()m ##return the cached inverse of x
	  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
			     
}



## The following function calculates the inverse of the matrix created above.
## It first checks if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips computation.
## Otherwise, it calculates the matrix inverse and sets the value of the inverse
## in the cache.



cacheSolve <- function(x, ...)
{
        ## Return a matrix that is the inverse of 'x'
	m <- x$getinverse()
	if(!is.null(m))
	{
	     message("getting the cached data")
	     return(m)
	}
	data <- x$get()
	m <- solve(data, ...)
        x$setinverse(m)
	m

}

