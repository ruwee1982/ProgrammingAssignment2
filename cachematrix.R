## this function will work for square matrices, using R's built in function `solve` to invert the matrix
# the <<- operator which can be used to assign a value to an object in an environment that is different from the current environment. Below are two functions that are used to create a special object that stores a numeric vector and cache's its mean.

# The first function, makeCacheMatrix creates a special "matrix" object, which is really a list containing a function to

# set the value of the matrix
# get the value of the matrix
# set the value of the inversed matrix
# get the value of the inversed matrix if cached

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
        x <<- y
        m <<- NULL
      }
      get <- function() x
      setinv <- function(inv) m <<- inv
      getinv <- function() m
      list(set = set, get = get,
           setinv = setinv,
           getinv = getinv)
}


## this function will try to get the inv of the matrix from makeCacheMatrix if it's cache, otherwise it will attempt to inverse the matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
          m <- x$getinv()
          if(!is.null(m)) {
            message("getting cached data")
            return(m)
          }
          data <- x$get()
          m <- solve(data, ...)
          x$setinv(m)
          m  
}

# testing the functions above
x1 <- makeCacheMatrix(matrix(c(1,2,7,8),2,2))

# check whether the inverse of the matrix exists or not
x1$getinv()

t1 = Sys.time()
cacheSolve(x1)
durationNoCache=Sys.time()-t1

t2 = Sys.time()
cacheSolve(x1)
durationWithCache=Sys.time()-t2

durationWithCache < durationNoCache

# the inverse of inversed of a matrix will be the original matrix
x1$set(x1$getinv())
cacheSolve(x1)