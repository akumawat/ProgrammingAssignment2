## Author 	: 	Anurag Kumawat
## Date		:	April 27, 2014

## Matrix inversion is usually a costly computation 
## and there may be some benefit in caching the inverse 
## of a matrix rather than compute it repeatedly.  
## This R script has a pair of functions that caches the 
## inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {

# This function creates a special "Matrix" object that   
# can cache its inverse.  
#  # Args:  
#   x: the input matrix whose inverse needs to be cached  
#  # Returns:  
#   A special "matrix", which is really a list containing a   
#   function to :-  
#	1. set the value of the matrix  
#   2. get the value of the matrix  
#   3. set the Inverse Matrix  
#   4. get the Inverse matrix    

# initialize the cached inverse value to NULL
    
	inverseMatrix <- NULL
	
	# to set the value of the matrix
	set <- function(y){
		x <<- y
		inverseMatrix <<- NULL
	}
	# to get the value of the matrix
	get <- function() x
	
	# to set the inverse
	setMatrixInverse <- function(matrix) inverseMatrix <<- matrix
	
	# to get the inverse
	getMatrixInverse <- function() inverseMatrix
	 
	# return a list of all the above functions
	list(set = set,get = get,
		setMatrixInverse = setMatrixInverse,
		getMatrixInverse = getMatrixInverse)
}

cacheSolve <- function(x,...){
# This function calculates the inverse of the special 
  # "matrix" created with the above function. However, it first 
  # checks to see if the inverse has already been calculated. 
  # If so, it gets the inverse from the cache and skips the 
  # computation. Otherwise, it calculates the inverse of the 
  # matrix and sets the value of the inverse in the cache via 
  # the setMatrixInverse function.
  #
  # Args:
  #   x: the input matrix whose inverse needs to be cached
  #
  # Returns:
  #   A matrix that is the inverse of 'x'
  
  # check if the inverse is already in the cache
	inverseMatrix <- x$getMatrixInverse()
	
	# if it exists return the cached inverse
	if(!is.null(inverseMatrix)){
		message("getting cached data")
        return(inverseMatrix)
	}
	
	# else get the matrix into matrixData
	data <- x$get()
	
	# and compute the inverse of matrix fetched
	inverseMatrix <- solve(x,...)
	
	# then cache the inverse
	x$setMatrixInverse(inverseMatrix)
	
	# and finally return the inverse
	inverseMatrix
}