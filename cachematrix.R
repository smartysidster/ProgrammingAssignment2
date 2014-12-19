## This function will create a liost of functions that will cache the inverse 
## of a matrix
makeCacheMatrix <- function(x = matrix()) 
{
        i <- NULL
        func1 <- function(y)
        {
                x <<- y
                i <<- NULL
        }
        func2 <- function()x
        makeInv <- function(inverse)i <<- inverse
        fetchInv <- function()i
        list(func1 = func1, func2=func2,makeInv=makeInv,fetchInv=fetchInv)
        
}
## For any matrix, if the inverse is already cached, this function will retrieve it from thr
## above function. Else it will comput the inverse of the matrix.
cacheSolve <- function(x, ...) 
{
        i <- x$fetchInv()
        if (! is.null(i))
        {
                return(i)
        }
        else
        {
                i<- solve(x$func2())
                x$makeInv(i)
                i
        }
}
