# functions to cache inverse of a matrix

makeCacheMatrix <- function( m = matrix() ) {

	# set the initial value of the inv
    s <- NULL

    # set the matrix
    set <- function(P) {
            m <<- P
    # put a null in inv
            s <<- NULL
    }

    # get the matrix
    get <- function() {
    	# Return the matrix
    	 m
    }

    # do the inverse of the matrix
    setinv <- function(inverse) {
       j <<- inverse
    }
    # this will return a list of the value of the matrix
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


# next part
cacheSolve <- function(x, ...) {

    # we return the matrix that is the inverse of 'x'
    m <- x$getinv()

    # make the function return the inverse if it is already set
    if( !is.null(m) ) {
            message("getting cached data")
            return(m)
    }

    # here we get the matrix from our object
    data <- x$get()

    # using matrix multiplication we calculate the inverse 
    m <- solve(data) %*% data

    # we set the inverse that we have to the object
    x$setinv(m)

    # Return the matrix
    m
}
