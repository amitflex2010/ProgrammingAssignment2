
#The below two functions have been written to calculate the inverse of the matrix. As matrix inversion is very costly 
#computation problem in terms of performance. So what is being done is to cache the inverse of the matrix in place of 
#computing every time. do

# makeCacheMatrix takes a matrix as parameter and create a list containing setter and getter for the matrix and setter 
# and gettter for inverse of the matrix.


makeCacheMatrix <- function(x = matrix()) {
m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


# The following function returns the inverse of the matrix. It first checks if
# the inverse has been computed already. If yes, it takes the already cached value and skips the
# computation. If no, it computes the inverse and sets the value in the cache via
# setinverse function.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		
		m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached inverse data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
