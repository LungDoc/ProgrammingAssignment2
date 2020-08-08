## Programming assignment 2
## Functions to cache the inverse of a matrix

## Creating a special "matrix" that will cache the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
        inv<-NULL
        set<-function(y) {
                x<<-y
                inv<<-NULL
        }
        get<-function() x
        setinv<-function(solve) inv<<-solve
        getinv<-function() inv
        list(set=set, get=get, setinv=setinv, getinv=getinv)
}

## Computing the inverse of the special "matrix" returned by makeCacheMatrix
## If inverse has already been created, the cache will be returned

cacheSolve <- function(x, ...) {
        inv<-x$getinv()
        if(!is.null(inv)){
                message("Getting cached data")
                return(inv)
        }
        data<-x$get()
        inv<-solve(data, ...)
        x$setinv(inv)
        inv
}
