## Assignment #2: Caching the inverse of a matrix

## makeCacheMatrix
## input:   invertible matrix 'x'
## output:  list of functions which act upon 'x'
##          (set, get, setsolve, getsolve)
##
## Return a list of functions which cache and retrieve values of matrix 'x' 
## and matrix 'inverse'.

makeCacheMatrix <- function(x = matrix()) {

    inverse <- NULL
    
    set <- function(y) {                ## set: assigns a value to 'x' (a matrix 
        x <<- y                         ##      in a different environment)
        inverse <<- NULL
    }
    
    get <- function(){                  ## get: retrieves 'x'
        x
    }
    
    setinvsolve <- function(invsolve){  ## setsolve: assigns a value to 
        inverse <<- invsolve            ##           'inverse' (a mattrix in a 
    }                                   ##           different environment)
    
    getinvsolve <- function(){          ## getsolve: retrieves the matrix 
        inverse                         ##           'inverse'
    }
    
    list(set = set, get = get,          ## outputs list of functions
         setinvsolve = setinvsolve,
         getinvsolve = getinvsolve)
    
}


## cacheSolve
## input: makeCacheMatrix acting on invertible matrix 'x'
## output: inverse of 'x'
##
## Return a matrix that is the inverse of 'x'
## If inverse of 'x' has been cached, output the cached value; 
## if not,calculate the inverse, cache and output it.

cacheSolve <- function(x, ...) {
    
    inverse <- x$getinvsolve()          ## Check to see if inverse is cached.
    
    if(!is.null(inverse)) {             ## If 'inverse' is not NULL, 
        message("getting cached data")  ## output cached value of 'inverse'.
        return(inverse)
    }
    
    else{
        data <- x$get()                 ## Retrieve matrix 'x'.
        inverse <- solve(data, ...)     ## Calculate the inverse of the matrix.
        x$setinvsolve(inverse)  ## Cache inverse of 'x', setting it to 'inverse' 
        return(inverse)                 ## Ouput 'inverse'.
    }
}
