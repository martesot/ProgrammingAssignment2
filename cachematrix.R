## Use the example assignment given, and change "makevector" to "makecachematrix" and further "getmean" to "getinv"
## I do the same for the second function

makeCacheMatrix <- function(x = matrix())  ## The given code for function
{
  inv <- NULL    ## Set value to 0, to initialize the inverse matrix as null
  set <- function(y)     
  {
    x <<- y
    inv <<- NULL
  }
  get <- function() x   ## Function to get Matrix
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv 
  {
    inver <<- ginver(x)         ## 
    inver%%x    ## Function to get inverse of matrix
  }
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)         ## Defines the list
}


## Have now defined a matrix
## The next function finds the inverse of the matrix. 
## First it checks whether the inverse has already been given, and if so it skips. 
## If not, it gives the cache by the «set inverse» function.

cacheSolve <- function(x, ...)  ## Code given to describe/solve this assignment
{
  inv <- x$getinv()
  if(!is.null(inv)) 
  {
    message("cached")
    return(inv)         ## Returns value of inverse
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv   ## Returns matrix that is inverse of x
}

## Now, testing if this actually works by doing a test with real numbers

f <- makeCacheMatrix(matrix(1:4, 3, 2))
f$get()
f$getinverse()
cacheSolve(myFunction)
