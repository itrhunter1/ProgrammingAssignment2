## Two fnctions, technically, (there are more within each function) that cache the inverse of matric

## This function creates a unique matrix object that can cache it's inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  # create null variable for the invers
  set <- function(y){ #assignment of the set
    x <<- y # double assignemnt to move y to Global Env.
    inv <<- NULL # create null variabel  for the iners in the Global Env.
  }
  get <- function() x
  setInverse <- function(solveMatrix) inv <<- solveMatrix
  getInverse <- function() inv
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This returns the computation of the unique matix from former function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse() 
  if(!is.null(inv)){ # check for null
    return(inv)
  }
  data <- x$get() # apply dataset
  inv <- solve(data) 
  x$setInverse(inv)
  inv  #return the inverse to the console
}
