## The functions calculate the inverse of a matrix and caches it to avoid
## repeating massive calculations

## makeCacheMatrix() function creates a list object, containing four functions
## that get the value of a matrix (get), set the value of a matrix (set),
## get the value of solve function (getsolve), set the value of solve function
## (setsolve) for the matrix

makeCacheMatrix <- function(x = matrix()){        
        inv <- NULL
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) inv <<- solve
        getsolve <- function() inv
        list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}

## cacheSolve() function checks if the inverse of a matrix has already been
## cached. If so the function returns the cached data, otherwise the function
## calculates the inverse and caches it.

cacheSolve <- function(x, ...){
        inv <- x$getsolve()
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setsolve(inv)
        inv
}