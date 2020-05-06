## The purpose of cachematrix.R is to take a square matrix, X,
## that is known to be invertible (alway invertible) and
## create a "matrix" object that can cache its own inverse.
##
## An example to make clear, Let M1 and M2 be two invertible square matrices.
## and M1Inv and M2Inv be the inverses of M1 and M2, respective.
## Recall that if I is the identity matrix, then M1 %*% M1Inv = I
#
#  So 
##
# If we run these two commands
#     a <- makeCacheMatrix(M1)
#     cacheSolve(a)
#
## we will get M1Inv as output
##
# The 'a' is a list of functions that we will pass into cacheSolve()
#
## if we ran this command again
#     cacheSolve(a)
# R will retrieve the cached or saved M1Inv
# No calcuations needed.
#
## Additionally, if we wanted to find the inverse for another matrix M2,
## All that we have to do is to set the matrix to be M2 by the command
#
#         a$set(m2)
#
#RERUN
#       cacheSolve(a)
# and we will get M2Inv as output.
#
#We will now describe the first function makeCacheMatrix()
#
## This function creates a list of functions that will take a matrix as input
# Additionally, we will be able to access each function using $FuncName
# The four functions are set, get, setInv, and getInv
#
# set() allows one to set the matrix whether it is in the initial call
# or whether the matrix changes when we set a new matrix.
#
# get() gets the matrix; setInv() obtains the setter for inverse;
# getInv() defines the getter for the inverse
# Last, a list is created with the four functions and
# and the list atributed the names to the rows too.
# 
makeCacheMatrix <- function(x = matrix()) {
  invMatrix <- NULL
  set <- function(y){
    x <<- y
    invMatrix <<- NULL
  }
  get <- function() x
  setInv <- function(solve) invMatrix <<- solve
  getInv <- function() invMatrix
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}

## The cacheSolve() function takes the list of functions created from 
# makeCacheMatrix and will produce the inverse of the matrix.  If the inverse
# has been calculated previously, then the cached inverse matrix will be used.
#
# This function will indicate when it goes and gets the invers.
#
# We are using solve() to get the inverse of the matrix.
#

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invMatrix <- x$getInv()
  if(!is.null(invMatrix)) {
    message("getting cached inverse matrix")
    return(invMatrix)
  }
  newMatrix <- x$get()
  invMatrix <- solve(newMatrix)
  x$setInv(invMatrix)
  invMatrix
}
