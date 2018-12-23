## makeCacheVector will will create a "special" vector to cache the inverse of 
## an input (which is an invertible square matrix) 

makeCacheMatrix <- function(x = matrix())  {
        m <- NULL
        set <- function(y)  {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInv <- function(solve) m <<- solve
        getInv <- function() m
        list(set = set, get = get, 
             setInv = setInv, 
             getInv = getInv)
}



##  This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already 
## been calculated (and the matrix has not changed), then cacheSolve 
## should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInv()
        if(!is.null(m))  {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInv(m)
        m
}
        