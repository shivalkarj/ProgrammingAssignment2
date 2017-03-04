#makeCacheMatrix function creates a speacial "matrix" object that can cache its inverse"

makeCacheMatrix <- function(x = matrix())
{
    
    m <- NULL
    set <- function(y=matrix()) 
    {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinvmatrix <- function(solve) m<<-solve
    getinvmatrix <- function() m
    list(set=set,get = get, setinvmatrix = setinvmatrix,getinvmatrix=getinvmatrix)
}

#cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix.
#Solve function in R is used to compute the inverse of a matrix
cacheSolve <- function(x, ...) 
{
    m <- x$getinvmatrix()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinvmatrix(m)
    m
}
