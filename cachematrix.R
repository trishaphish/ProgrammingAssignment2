## makeCacheMatrix helps create a matrix that can store a copy of it's inverse


makeCacheMatrix <- function(x = matrix()) {
## this creates a matrix that functions as a list.
## inv_mat represents the matrix, set_inv is a function to set the inverse
## get_inv calls the saved matrix and will be used in cacheSholve below
        
        inv_mat <- NULL
        set <- function(y) {
                x <<- y
                inv_mat <<- NULL
        }
        get <- function() x
        set_inv <- function(inverse) inv_mat <<- inverse
        get_inv <- function() inv_mat
        
        list(
                set = set,
                get = get,
                set_inv = set_inv,
                get_inv = get_inv
        )
}


## cacheSolve creates an inverse of a matrix.  But first, it checks against
## makeCacheMatrix for a saved inverse matrix file.

cacheSolve <- function(x, ...) {
        inv_mat <- x$get_inv()
        if(!is.null(inv_mat)) {
                message("getting cached data")
                return(inv_mat)
        }
        data <- x$get()
        inv_mat <- solve(data, ...)
        x$set_inv(inv_mat)
        inv_mat
}
