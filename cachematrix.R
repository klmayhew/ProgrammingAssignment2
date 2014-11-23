## Put comments here that give an overall description of what your
## functions do


cachedMatrix <- NULL
cachedInverse <- NULL

## function that creates and caches a matrix if it does not already exist in the cache
# if there is no cached matrix : cache the passed in matrix
# if the cached matrix does not equal the passed in matrix : cache the passed in matrix
# if the cached matrix equals the passed in matrix : do nothing

makeCacheMatrix <- function(x = matrix()) {
    
    if(is.null(cachedMatrix)){
        
        #print("no cached matrix : caching the provided matrix")
        cachedMatrix <<- x
    
    }else if(checkMatrixEquality(cachedMatrix, x)==F){
    
        #print("cached matrix does not equal the provided matrix: caching the provided matrix")
        cachedMatrix <<- x
        
    }else {
        #print("cached matrix equals the provided matrix: doing nothing") 
    }
    
    get <- function() cachedMatrix

    list(get = get)
}

## Return a matrix that is the inverse of 'x'
# if the cached matrix does not exist : create the matrix and inverse, cache them both
# if the cached matrix does not equal the passed matrix : cache the passed matrix, compute and cache the inverse
# if the cached matrix equals the passed matrix : return the cached inverse if it exists, create and cache the inverse if 
#    it does not exist

cacheSolve <- function(x, ...) {
    
    # if the cached matrix does not exist : cache the passed in matrix and its inverse
    if(is.null(cachedMatrix)){
        # print("no cached matrix : caching the passed matrix and its inverse")
        cachedMatrix <<- x$get()
        cachedInverse <<- solve(x$get())
    }    
    
    # if the cached matrix does not equal the passed matrix : cache the passed matrix, compute and cache the inverse
    if(checkMatrixEquality(cachedMatrix, x)==F){
        # print("cached matrix is different : caching the passed matrix and its inverse")
        cachedMatrix <<- x$get()
        cachedInverse <<- solve(x$get())
    }
    
    #if the cached matrix equals the passed matrix : return the cached inverse if it exists, create and cache the inverse if 
    #    it does not exist
    if(checkMatrixEquality(cachedMatrix, x$get())==T){
        # print("cached matrix is the same")
        if(is.null(cachedInverse)){
            # print("cached inverse does not exist, creating and caching it")
            cachedInverse <<- solve(x$get())
        }
    }
    
    cachedInverse
}

# check for equality of two matrices 
checkMatrixEquality <- function(first, second){
    
    is.matrix(first) && 
    is.matrix(second) && 
    dim(first) == dim(second) && 
    all(first == second)
}


testMakeCacheMatrix <- function(){
    
    x <- makeCacheMatrix(matrix(1:16, ncol=2))
    x <- makeCacheMatrix(matrix(1:16, ncol=2))
    x <- makeCacheMatrix(matrix(2:17, ncol=2))
    
}

testRequirements <- function(){
    m <- makeCacheMatrix(matrix(1:4, nrow=2, ncol=2))
    cacheSolve(m)
}