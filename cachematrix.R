#Create the following 2 functions
# 1) makeCacheMatrix
# 2) cacheSolve

#Part 1
#We will use the mass package to be able to compute the inverse of matrices. 
#Define the argument, set the inverse to be null & define the function.

library(MASS)
makeCacheMatrix <- function(x = numeric()) { 
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  #set the value in our environment so it can be called and build a list. Use ginv to compute the inverse of our matrices.
  
  get <- function()x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function(){ 
    inver <- ginv(x)
    inver%*%x
  }
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

#Part 2: Now we will make a cache that will be able to retrieve this special "matrix" inverse.

# Define the argument and check if the inverse is NULL.

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  #Calculate the inverse values.
  
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}

#Test the function.

test <-makeCacheMatrix(matrix(1:6,2,3))
test$getinv()
cacheSolve(test)