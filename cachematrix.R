makeCacheMatrix <- function(x = numeric()) {
      
      
      cache <- NULL #to store the inverted matrix,  when computed
      
      # store a matrix
      setMatrix <- function(newValue) {
            x <<- newValue #assign newValue to the x
            cache <<- NULL #if cache contained it is no longer relevant
      }
      
      getMatrix <- function() { #this returns the stored matrix
            x
      }
      
       
      cacheInverse <- function(solve) { #this is to cache the argument 
            cache <<- solve
      }
      
      getInverse <- function() { #returns inverted value from cache
            cache
      }
      
      list(setMatrix = setMatrix, getMatrix = getMatrix, cacheInverse = cacheInverse, getInverse = getInverse)
}


# The following function calculates the inverse of a "special" matrix created with 
# makeCacheMatrix
cacheSolve <- function(y, ...) {

      # if a cached value exists return it
      if(!is.null(y$getInverse())) {
            message("getting cached data")
            return(y$getInverse())
      }
      
      # otherwise get the matrix, caclulate the inverse and store it in
      # the cache
      data <- y$getMatrix()
      inverse <- solve(data)
      y$cacheInverse(inverse)
      
      # return the inverse
      inverse
}
