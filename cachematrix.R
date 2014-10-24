## For the rprog-008 course
## These are two functions for the Programming Assignment 2: Lexical Scoping

## First, caching the inverse of a matrix
## makeCacheMatrix creates a special 'vector' (returns a list object) that 
## stores a matrix vector and caches its inverse (calculated using 
## the solve() function).
## The makeCacheMAtrix is a direct copy of the makeVector one described in the
## Coursera assignment.         

makeCacheMatrix <- function(x = matrix()) {
        ## reset contents of m to be NULL - empty vector
        m <- NULL
        ## now call function within function to update x and m using special
        ## assignment operator <<-  looking through parent environment (first
        ## function, then global environment if not found.)
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        ## the easy bit - calculate inverse using solve
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Second, calculate the inverse of the special "vector', to go back to the
## orginal matrix.  But only if the inverse has already been calculated.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
