

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  # this will store the inverse
  
  # set a new matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL  # clear cached inverse
  }
  
  # get the matrix
  get <- function() x
  
  # set the inverse
  setinverse <- function(inverse) inv <<- inverse
  
  # get the inverse
  getinverse <- function() inv
  
  # return a list of functions
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
  inv<-x$getinverse()
  if(!is.null(inv)) {
    message ("getting cached data")
    return (inv)
  }
    mat<-x$get()
    inv<-solve(mat,...)
    x$setivnerse(inv)
    inv
}
# Create a matrix
my_matrix <- matrix(c(2, 1, 5, 3), nrow = 2, ncol = 2)

# Wrap it in makeCacheMatrix
cachedMatrix <- makeCacheMatrix(my_matrix)

# First time: calculates and caches
inverse1 <- cacheSolve(cachedMatrix)

## retrieves from cache
inverse2 <- cacheSolve(cachedMatrix)

