## This R script defines a function that returns an R object that can
## cache the inverse of an invertible square matrix.  It also provides
## a function that inverts the matrix contained in the object, or
## returns the cached already inverted matrix if the matrix hasn't
## changed

## The makeCacheMatrix function returns an R object that can cache the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
    # Define a variable to hold the cached inverse and set to NULL
    # as no inverse has yet been calculated
    inverse <- NULL
    # The set function allows a matrix to be set within the object,
    # which invalidates the cached inverse so we reset it to NULL
    set <- function(y)  {
        x <<- y
        inverse <<- NULL
    }
    # The get function returns the matrix
    get <- function() x
    # The setinverse function allows us to cache the inverse
    setinverse <- function(inverted) inverse <<- inverted
    # The getinverse function returns the cached inverse of the matrix
    getinverse <- function() inverse
    # We now create the R object that provides access to the cached
    # inverse
    list(set = set,
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## The cachSolve function returns the inverse of a matrix held in
## a cachMatrix object.  Preferably by returning the cached copy
## or if the inverse has not yet been calculated it will calculate
## and cache the inverse before returning it.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    # Get the cached inverse from x
    inverse <- x$getinverse()
    # Check whether we have alredy calculated the inverse
    if(!is.null(inverse)) {
        # If so then return it
        message("using cached inverse")
        return(inverse)
    }
    # otherwise we need to calculate the inverse
    # So get the matrix from the object
    m <- x$get()
    # Invert it using the solve function
    inverted <- solve(m)
    # Store it back in the object so we can use this cached version
    x$setinverse(inverted)
    # Use autoreturn to return the inverse
    inverted
}
