## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## The makeCacheMatrix function takes a vector of 4 numbers and returns four functions:
## 1. set: to convert the vector into 2 x 2 matrix, 2. ge: to return the matrix 3. setinv: that sets
## the inverse matrix and 4. getinv: That returns the inverse matrix.
makeCacheMatrix <- function(x = numeric()) {    #Intialize the function makeCacheMatrix and x as a numeric 
        #vector.
        x <- matrix(x,nrow = 2,ncol =2) #convert x into a 2 x 2 matrix
        m <- NULL                       # set m as null. 
       # set unction is defined below. This takes a vector or matrix y and converts into a 2 x 2 matrix.
       # since we use <<- assignment operator, the value of x is set in the parent environment of this funciton.
       # Similarly, value of m is set to null in the parent environment, i.e., function makeCacheMatrix.
        # Setting m to Null also ensures that any previously stored value of m is reset, should the matrix change.
        set <- function(y) {
                x <<- matrix(y,nrow = 2, ncol =2)
                m <<- NULL
        }
        get <- function() x   #Defines the get function and returns the matrix x
        setinv <- function(y)  m <<- y  #This sets the inverse matrix into m in the parent environment
        getinv <- function() m   # This returns the inverse matrix
        # the code below results in  the makeCacheMatrix to return a list containing these four functions.
                list(set = set, get = get, 
                     setinv = setinv,
                     getinv = getinv)
}

# This is the desktop copy
## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinv() # This line gets the matrix using the function getinv 
        #The code below checks whether the inverse matrix variable is NULL. If not, it gets 
        #the value of inverse matrix from the cache and returns it.
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        ## if m is NULL, it calculates the inverse.
       datamat <- x$get()  # first gets the matrix
       m <- solve(datamat) # calculate the inverse
       x$setinv(m)         # sets the value of inverse
       m                   # return inverse  
        }

