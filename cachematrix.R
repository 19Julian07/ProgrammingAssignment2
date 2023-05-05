# Creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
        
        # Create a vector to store the inverse
        inv <- NULL
        
        # Define a function to set the matrix
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        # Define a function to get the matrix
        get <- function() x
        
        # Define a function to get the inverse
        getinv <- function() inv
        
        # Define a function to set the inverse
        setinv <- function(inverse) inv <<- inverse
        
        # Return a list with the above functions
        list(set = set, get = get, getinv = getinv, setinv = setinv)
}

# Computes the inverse of the special "matrix" returned by makeCacheMatrix
cacheSolve <- function(x, ...) {
        
        # Get the inverse from cache if it exists
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        
        # Otherwise, calculate the inverse and store it in the cache
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        
        # Return the inverse
        inv
}
