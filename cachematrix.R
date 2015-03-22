## Put comments here that give an overall description of what your
## functions do

## Container function for holding the base matrix and the inverted matrix; 
## includes functions for getting and setting both matrices 
makeCacheMatrix <- function(x = matrix()) {
    #Cache to hold the inverted matrix
    invertedMatrix <- NULL;
    
    #Function for setting a new base matrix
    set <- function(y) {
        x <<- y
        invertedMatrix <<- NULL  #Since base matrix has changed, negate the inverted matrix
    }
    
    #Function for getting the base matrix
    get <- function() {
        x
    }
    
    #Function for retrieving the inverted matrix
    getInvertedMatrix <- function() {
        invertedMatrix
    }
    
    #Function for setting the inverted matrix
    setInvertedMatrix <- function(w) {
        invertedMatrix <<- w
    }
    
    list(set = set, get = get, 
         setInvertedMatrix = setInvertedMatrix, 
         getInvertedMatrix = getInvertedMatrix)
}


## Calculates (if required) and returns the inverse of matrix x
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    invert <- x$getInvertedMatrix()
    
    #Check if the inverted matrix is cached
    if (!is.null(invert)) {
        print("Using cached inverted matrix")    
    } else {
        print("Calculating inverted matrix")
        invert <- solve(x$get())
        # Cache the inverted matrix for future use
        x$setInvertedMatrix(invert);
    }
    
    invisible(invert)
}
