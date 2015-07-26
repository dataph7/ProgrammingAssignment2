## Put comments here that give an overall description of what your
## functions do

## Here, we are creating two functions which will create inverse of
## a matrix and cache its inverse. 

## Write a short comment describing this function

## Below function will create a special "matrix" which is a list of 
## containing set, get, setInv and getInv

makeCacheMatrix <- function(mat = matrix()) {
        Inv <- NULL
        set <- function(x){
                mat <<- x
                Inv <<- NULL
        }
        get <- function() mat
        setInv <- function(matInv) Inv <- matInv
        getInv <- function() Inv
        list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## Write a short comment describing this function

## cacheSolve function will checks if already the inverse of 
## the matrix is stored in the cache. If not, it calculates
## the inverse of the matrix

cacheSolve <- function(mat, ...) {
        ## Return a matrix that is the inverse of 'x'
        Inv <- mat$getInv()
        if(!is.null(Inv)){
                message("getting cached data")
                return(Inv)
        }
        data <- mat$get()
        Inv <- solve(data,...)
        mat$setInv(Inv)
        Inv
}