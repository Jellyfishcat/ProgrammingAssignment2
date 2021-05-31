### This 2 functions is use to cache the inverse of a matrix


## 1. makeCacheMatrix: creates a special "matrix" object that can cache its inverse.
##    it comes out an environment with 4 functions and 2 objects x & inv
##    this is the preparation for the next cacheSolve function, 
##    and the station where we cache the data I think...?

makeCacheMatrix <- function(x = matrix()){
      inv <- NULL
      set <- function(y){
            x <<- y   #(1)
            inv <<- NULL   #(2)
      }
      get <- function() x  #(3)
      setinv <- function(matri) inv <<- matri  #(4)
      getinv <- function() inv
      list(set = set,   #(5)
           get = get,
           setinv = setinv,
           getinv = getinv)
}

# (1) Use '<<-' point to assign the object in parent environment 
#     y is in set function's env, while x si in makeCacheMatrix function's env (the parent env)
# (2) To clear any value of inv that had been cached by a prior execution of cacheSolve()
# (3) The lexical scoping features in R: 
#     x is not defined within get(), but R can retrieve it from the parent environment of makeCacheMatrix.
# (4) can't just function(inv) without assginment, since that means defining a new argument names 'inv', 
#     but actually this 'inv' it is totally different from the inv in its parent environment. just the same name.
# (5) gives names to functions defined above. then we can use these functions by '$'. 


## 2. cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##    If the inverse has already been calculated (and the matrix has not changed), 
##    then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x,...){
      inv2 <- x$getinv()
      if(!is.null(inv2)) {    #(1)
            message("getting cached data")
            return(inv2)
      }
      data <- x$get() 
      inv2 <- solve(data, ...)  #(2)
      x$setinv(inv2)  # (3)
      inv2
}      

# (1) To find whether the inverse matrix has been cached
# (2) If not, calculate to produce a new inverse matrix
# (3) And cache the matrix in inv from makeCacheMatrix's environment.
