## First function is designed to cache the inverse of a matrix calculted in the second function.
## Thus, when executing the second function several times for the same matrix, calculaitons will be skipped.

## makeCacheMatrix creates a list of 4 functions: 
##      1.set the value of the matrix
##      2.get the value of the matrix
##      3.set the value of the matrix inverse
##      4.get the value of the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
    m_inv <- NULL # returns an inverse matrix as NULL meaning nothing is cached yet 
    set <- function(y) { # sets a new matrix to calculate and cache inverse
        x <<- y
        m_inv <<- NULL
    }
    get <- function() x # is setup to get the matrix to calculate the inverse by calling in second function
    setm_inv <- function(solve) m_inv <<- solve # saves cache for new matrix
    getm_inv <- function() m_inv # is setup to call in second function in case inverse is cached
    list(set = set, get = get, #returns a list of 4 above functions
         setm_inv = setm_inv,
         getm_inv = getm_inv)
}


## cacheSolve returns an inverse of the matrix. It is returned from makeCacheMatrix if existed.

cacheSolve <- function(x, ...) {
    m_inv <- x$getm_inv() # saves (temporary) a cache or NULL if nothing exists
    if(!is.null(m_inv)) {
        message("getting cached data")
        return(m_inv) # returns the cache if exists
    }
    data <- x$get() # gets a new matrix
    m_inv <- solve(data, ...) #calculates an inverse for a new matrix
    x$setm_inv(m_inv) # cache inverse from above
    m_inv #returns an inverse 
}
