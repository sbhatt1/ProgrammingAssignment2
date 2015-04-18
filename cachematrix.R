
#############################################################################################
# Note: If you would like to check results, just source this file, it would define functions
# and then run few examples to show how it is working
#############################################################################################

#define a 2x2  matrix called "matrix1" in env
matrix1 <- matrix(c (1,2,9,8), nrow = 2, ncol = 2)

# define a 2x2 NULL matrix called "invert_matrix1; in env 
# and then assign it to NULL
invert_matrix1 <- matrix(data = NA, nrow = 2, ncol = 2)
invert_matrix1 <- NULL

#####################################################################################################################
# makeCacheMatrix function takes a matrix as an argument and the returns
# a list containing various functions to manipulate the matrix.
# The list restured by the makeCacheMatrix has following functions
# 
# set_matrix -- This function takes in a matrix as an argument and then assigns matrix
#            -- passed through the argument to 'matrix1' which is stored in global environment
#            -- it also sets the 'invert_matrix1' (which is the invert of 'matrix1') in glbal environment to "NULL"
#            -- this is done since the cached value of 'invert_matrix' is not valid anymore since 
#            -- 'matrix1' has changed
#
#
# get_matrix --  This function returns the 'matrix1' which is stored in global enviornment
# set_invert --  this function takes a matrix as an argument and assigns the argument matrix to "invert_matrix1'
#            --  this function is called to cache the calculated to invert matrix value
# get_invert -- this function returns cached inverted matrix value from 'invert_matrix1'
#
#####################################################################################################################

makeCacheMatrix <- function(x = matrix()) {
  invert_matrix <- NULL
  set_matrix <- function(y = matrix()) {
    matrix1 <<- y
    invert_matrix1 <<- NULL
  }
  get_matrix <- function() matrix1
  set_invert <- function(invert_matrix_arg) invert_matrix1 <<- invert_matrix_arg
  get_invert <- function() invert_matrix1
  list(set_matrix = set_matrix, get_matrix = get_matrix,
       set_invert = set_invert,
       get_invert = get_invert)
}


################################################################################################
#  Cacheresolve function takes a matrix as vector list returned form the makeCacheMatrix function
#  as an argument. 
#  If an inverted matrix value is already calculated (stroed in global env invert_matrix1),
#  the function returns the cached inverted value with a message. However, if invert_matrix1 is 'null'
#  that means that there is no cached value avaialble, the funciton calculated the inverted value
#  and then caches it in the 'invert_matrix1' by calling set_invert() function
#
################################################################################################

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$get_invert()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  message("cached data not available!!! calculating")
  local_m <- x$get_matrix()
  x$get_matrix()
  local_invert_m <- solve(local_m)
  x$set_invert(local_invert_m)
  local_invert_m
}
##########################################################################################
# Code below uses makeCacheMatrix() and cacheSolve() functions to demonstrate working
#########################################################################################

#run for the first time. At this time since invert_matrix1 is NULL, casheSolve function
# calculates the inverted value and then stores it in invrt_matrix1 
matrixVector <- makeCacheMatrix(matrix1)
print(cacheSolve(matrixVector))

#run second time. At this time since invert_matrix1 already contains inverted matrix,
# cached value is returned

print(cacheSolve(matrixVector))

# Now, set the matrix1 to a new value. This is done through set_matrix() function
# that is defined as part of list returned by makeCachematrix()
# This will set the invert_matrix1 to NULL and hence cached value is wiped clean.

message("---------------------------------------------------------------------------")
message("Now setting matrix1 to new value matrix (c(11,12,13,21), nrow =2, ncol =2)")
message("---------------------------------------------------------------------------")

matrixVector$set_matrix(matrix (c(11,12,13,21), nrow =2, ncol =2))

# Run first time with new value. This time inverted value is calculated and stored in invert_matrix1
print(cacheSolve(matrixVector))

#Run 2nd time with new value. This time cached value is returned.
print(cacheSolve(matrixVector))
