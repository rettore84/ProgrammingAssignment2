# makeCacheMatrix: This function creates a special "matrix" object 
# that can cache its inverse.

#This function creates a special "vector", which is
#really a list containing a function to

# 1.  set the value of the vector
# 2.  get the value of the vector
# 3.  set the value of the inverse matrix
# 4.  get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  set_inv <- function(solve) m <<- solve
  get_inv <- function() m
  list(set = set, get = get,
       set_inv = set_inv,
       get_inv = get_inv)
}
  
  



# The following function calculates the inverse of the special "vector"
# created with the above function. However, it first checks to see if the
# inverse matrix has already been calculated. If so, it `get`s the inverse from the
# cache and skips the computation. Otherwise, it calculates the inverse matrix of
# the data and sets the value of the inverse matrix in the cache via the `set_inv`
# function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$get_inv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()#matrix(c(1,3, 2,0), nrow = 2, ncol = 2, byrow = TRUE)
  m <- solve(data, ...)
  x$set_inv(m)
  m
  
}
