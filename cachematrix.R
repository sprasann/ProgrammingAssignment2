?## returns the inverse of a square invertible matrix
## caches the matrix so that inverse need not be repeatedly calucated
## thus saving time of computing inverse


## creates a special "matrix" object that can cache its inverse
## takes a square invertible matrix as an input and returns a list

makeCacheMatrix <- function(x = matrix()) {
        
        invert <- NULL
        
        ## sets the matrix
        setMatrix <- function(y){
                x  <<- y
                invert <<- NULL
        }

        ## returns the Matrix 
        getMatrix <- function() x
        
        ## sets the inverse of the matrix in "someother" environment
        setInverse <- function(inv) {
                invert <<- inv
        }
        
        ## gets the inverse from "someother" environment
        getInverse <- function() invert
        
        l <- list(setMatrix = setMatrix, 
                     getMatrix = getMatrix,
                     setInverse = setInverse,
                     getInverse = getInverse)
        
        return(l)
}


## computes the inverse of the special "matrix" returned by makeCacheMatrix 
## above. If the inverse has already been calculated, then the cachesolve 
## would retrieve the inverse from the cache.

## takes the special matrix returned by makecacheMatrix as an input and 
## returns the inverse matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        
        ## gets the inverse from "someother" environment
        invert <- x$getInverse()
        

        ## if the inverse is present returns it
        if(!is.null(invert)){
                print("returning from cache")
                return(invert)
        }
        
        mat = x$getMatrix()
        
        print("calculating inverse for the first time")
        invert <- solve(mat, ...)
                                
        ## setting the cache
        x$setInverse(invert)
        
        return(invert)
}

