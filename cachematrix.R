## Caching inverse of a matrix
## Matrix inversion generally involves costly computation
## there is benefit in caching the inverse of a matrix and 
## avoiding computing it repeatedly

## the two functions below create an object that stores
## a matrix and caches its inverse

makeCacheMatrix <- function(x = matrix()) {
        inv<- NULL
        set<- function(y){
           x<<- y
          inv<<- NULL
        }
        get<- function() x
        setInverse<- function(inverse)inv<<- inverse
        getInverse<- function()inv
        list(set=set,
             get=get,
             setInverse=setInverse,
             getInverse=getInverse)

}


## The function computes the inverse of the special matrix 
## created by the function above
## If the inverse has already been calculated, then the
## it should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <-x$getInverse()
        if(!is.null(inv)){
            message("getting cached data")
            return(inv)
        }
        mat <-x$get()
        inv <-solve(mat,...)
        x$setInverse(inv)
        inv
}
