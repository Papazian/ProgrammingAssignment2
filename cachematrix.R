## I have written comments to give an overall description of what my two functions do

# The first function, makeCacheMatrix() creates a special "matrix", which is really a list containing 4 functions:
# (1) set the value of the matrix
# (2) get the value of the matrix
# (3) set the value of the inverse of the matrix
# (4) get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) 
{
  i  <- NULL
  set <- function(y) 
  {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse  <- function(solve) i  <<- solve
  getinverse  <- function() i 
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

# The second function cacheSolve() calculates the inverse of the special "matrix" created with the above function. 
# However, it first checks to see if the inverse of the matrix has already been calculated. 
# If so, it gets the inverse from the cache and then skips the computation. 
# Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) 
{
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) 
  {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}

# Now lets test these functions. First, we test out computing an inverse normally by solving a matrix.

test1 = matrix(c(4, 3, 3, 2), nrow=2, ncol=2)
solve(test1)

# Then, we test by using the cacheSolve() function on a "matrix" created using the makeCacheMatrix() function

test2 <- makeCacheMatrix(matrix(c(4, 3, 3, 2), nrow=2, ncol=2))
cacheSolve(test2)

# Finally, we test by using the cacheSolve() function on the same matrix where the inverse has been cached.

cacheSolve(test2)
