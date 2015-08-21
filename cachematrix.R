## makeCacheMatrix function stores a list of functions
## set: this function which changes the object (attribute x) stored in the main function
## get: this is a function which simply returns what's stored in the attribute x
## setmean: this function stores the value of the input x in a variable m into the main function
## getmean: this function returns the value of the variable m

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(z) m <<- z
        getinverse <- function() m

        #create a list to store the above four functions
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Input of cacheSolve is the object where makeCacheMatrix is stored e.g.
## a <- makeCacheMatrix(matrix(1:4,2,2))
## cacheSolve(a)
## cacheSolve takes in a matrix, if that matrix has been passed in before then it will return it's "inverse"
## from cache, else it will calculate it's "inverse", store it in the cache and return the "inverse"
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        #retrieve the value of the matrix and store it in the data variable
        data <- x$get()
        m <- solve(data, ...) #the solve function inverts a matrix
        x$setinverse(m) #store the value of the inverted matrix in the cache for future use
        m
}
