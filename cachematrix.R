# This second programming assignment will require you to write an R function that is able to cache potentially time-consuming 
# computations. For example, taking the mean of a numeric vector is typically a fast operation. However, for a very long vector, 
# it may take too long to compute the mean, especially if it has to be computed repeatedly (e.g. in a loop). If the contents of a 
# vector are not changing, it may make sense to cache the value of the mean so that when we need it again, it can be looked up in the 
# cache rather than recomputed. In this Programming Assignment you will take advantage of the scoping rules of the R language and 
# how they can be manipulated to preserve state inside of an R object.

#The first function, makeCacheMatrix creates a special "vector", which is really a list containing a function to

#set the value of the vector
#get the value of the vector
#set the value of the mean
#get the value of the mean


makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  setValue <- function(y) {
    x <<- y
    i <<- NULL
  }
  getValue <- function() x
  setInverse <- function(inverse) i <<- inverse
  getInverse <- function() i
  list(set = setValue, get = getValue, 
       setInverse = setInverse, 
       getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
       i <- x$getInverse()
        if(!is.null(i)) {
         message("Getting cached matrix data")
          return(i)
        }
      data <- x$get()
      i <- solve(data)
      x$setInverse(i)
      i
}
# Run Examples
# > x <-matrix(1:4,nrow=2, ncol=2)
# > a = makeCacheMatrix(x)
# > a$get()
# [,1] [,2]
# [1,]    1    3
# [2,]    2    4
# > cacheSolve(a)
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
# > cacheSolve(a)
# Getting cached matrix data
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
# > 