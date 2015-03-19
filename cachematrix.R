## Description

## This function creates a special "matrix" object 
##  that can cache its inverse (using the solve function)

makeCacheMatrix <- function( x = matrix() ) {

## we will assume the matrix is square 

        inverse <- NULL               ## initialize the obj's $inverse variable        

        setmatrix <- function(y) {    ## function to set the inner matrix obj.
        
                x <<- y               ## reset the "x" matrix using "y"
                
                inverse <<- NULL      ## clear out the inverse since we 
                                      ## just changed the matrix
                                      ## that will force a re-calculation of 
                                      ## the inverse
        }

        getmatrix <- function() { 
            x 
        }
        
        setinverse <- function(solve) {  ## this is how we get the inverse 
                                         ## matrix back from cacheSolve
            inverse <<- solve            ## move the result into this 
                                         ## function's environment
        }
        
        getinverse <- function() {       ## used to get the inverse
            inverse 
        }
        
        list(setmatrix = setmatrix, 
             getmatrix = getmatrix,
             setinverse = setinverse,
             getinverse = getinverse)

}


 ## Description
 ## This function computes the inverse of the special square "matrix" 
 ##   returned by makeCacheMatrix above. 
 ## If the inverse has already been calculated (and the matrix has not 
 ##  changed), then cacheSolve should retrieve the inverse from the 
 ##  cache.
 
cacheSolve <- function(x, ...) {

        ## Return a matrix that is the inverse of 'x'

        inverse <- x$getinverse()            ## get the inverse from x

        if(!is.null(inverse)) {              ## check if the inverse was already
                                             ## set (it won't be null if it was
        
            message("getting cached data")   ## this message is displayed 
                                             ## the second time you try to 
                                             ## run cacheSolve()

            return(inverse)                  ## returns the inverse
        
        }

        data <- x$getmatrix()                ## set's the data variable to the 
                                             ## actual matrix we started with

        inverse <- solve(data, ...)          ## calculate the matrix inverse

        x$setinverse(inverse)                ## set the matrix's inverse

        inverse                              ## return the inverse
        
}






