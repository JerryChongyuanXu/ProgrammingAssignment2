## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function


## The first function, makeCacheMatrix creates a special matrix, 
## which is a list containing a function to set and get the value of 
## both the matrix and its inverse. 
makeCacheMatrix <- function(x = matrix()) {
	
	## Initialize the inverse property
	m <- NULL
	
	## The Method to set the matrix
	set <- function(y){
		x <<- matrix
		m <<- NULL
	}
	
	## The Method to get the matrix
	get <- function() x
	
	## The Method to set the inverse of the matrix
	setInverse <- function(inverse) m <<- inverse
	
	## The Method to get the inverse of the matrix
	getInverse <- function() m
	
	## Return all the methods before
	list(set = set, get = get, 
	     setInverse = setInverse, 
	     getInverse = getInverse)
}


## Write a short comment describing this function


## This function calculates the inverse of the special matrix 
## created with the above function. Firstly, it checks if the 
## inverse has been calculated. If so, it gets the inverse from the 
## cache and skips the computaion. Otherwise, it calculates the 
## inverse of the matrix and sets the inverse of the matrix in the 
## cache according to the setmatrix function. 
cacheSolve <- function(x, ...) {
	
    ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()
    
    ## Return the inverse if the inverse has already been calculated
    if( !is.null(m) ){
    	message("getting cached data")
    	return(m)
    }    
    
    ## Get the matrix from the object
    data <-x$get()
    
    ## Calculate the inverse of the matrix
    m <- solve(data)
    
    ## Set the inverse to the object
    x$setInverse(m)
    
    ## Return the matrix
    m
}
