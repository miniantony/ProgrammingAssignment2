## Put comments here that give an overall description of what your
## functions do

## Below are two functions used to create a special object that stores a matrix and cache's its inverse.

## Short comment describing this function

## The first function, makeCacheMatrix creates a special "matrix", 
## which is really a list containing a function to:
## 1) set the value of the matrix (set_matrix)
## 2) get the value of the matrix (get_matrix)
## 3) set the value of the inverse (set_inverse)
## 4) get the value of the inverse (get_inverse)

makeCacheMatrix <- function(x = matrix()) {                   ## define the argument with default mode of "matrix"
        inverse <- NULL                  		      ## Initialize inverse as 'NULL', holds the value of inverse matrix 
        set_matrix <- function(y) {		              ## define the set function to assign new
        x <<- y 	  		                      ## value of matrix in parent environment as matrix 'x'
        inverse <<- NULL                                      ## if there is a new matrix, reset inv to NULL
        }
        get_matrix <- function() x 			      ## define the get function - returns value of the matrix argument'x'
        set_inverse <- function(solve) inverse <<- solve      ## assigns value of inverse in parent environment
        get_inverse <- function() inverse   	              ## gets the value of inverse where called
        list(set_matrix = set_matrix, get_matrix = get_matrix,
        set_inverse = set_inverse, get_inverse = get_inverse)  ## needed to refer to the function with the $ operator
}

## Short comment describing this function

## The below function calculates the inverse of the special "matrix" created by the above makeCacheMatrix function.
## It first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via 
## the 'set_inverse' function.

cacheSolve <- function(x, ...) {                        ## Return a matrix that is the inverse of 'x'
       inverse <- x$get_inverse()			## Getting inverse
       if(!is.null(inverse)) {				## Checking for the presence of inverse
                message("getting cached data")		## Displaying message
                return(inverse)
        }
        data <- x$get_matrix()			        ## Getting Matrix
        inverse <- solve(data, ...)	        	## Using solve() to compute inverse
        x$set_inverse(inverse)				## To cache the inverse
        inverse 					## Returning the inverse
}
