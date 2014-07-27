######### 
#This assignment consists of creating 2 functions, one that caches a matrix and its inverse, another that computes the cached results, and if none computed before, returns the result (inversed matrix).
#########

###Useful resources prior to solving this assignment:
#Source: http://personality-project.org/r/sem.appendix.1.pdf
#Source: http://stackoverflow.com/questions/15575240/in-r-programming-concerning-on-inverse-matrix-and-its-multiplication


#The first function, makeCacheMatrix creates a special "matrix" object, which can cache its inverse. It contains:
#a) set the value of original matrix
#b) get the value of the original matrix
#c) set the value of the inverse-matrix (the assignment assumes that the original matrix is inversable)
#d) get the value of the inverse-matrix

makeCacheMatrix <- function(x = matrix()) {
     #initially sets inverse matrix to null:
	i <- NULL
	#Sets the value of the original matrix:
     set <- function(y) {
             x <<- y
             i <<- NULL
     }
	#Gets the value of the original matrix:
     get <- function() x
	
	#Calculates the inverse of the original matrix
     setInverse <- function(solve) i <<- solve
     getInverse <- function() i
	
	#Return a list of data
     list(set = set, get = get,
          setInverse = setInverse,
          getInverse = getInverse)
}


## The following function (cacheSolve) takes the cached matrix created with the above function (makeCacheMatrix) as an argument. However, it first checks to see if the inverse of the original matrix has already been calculated. If so, it gets the inverse from the cache and skips the computation. Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the setInverse function.

cacheSolve <- function(x, ...) {
	   
	   #Get the inverse value stored in cache:
        i <- x$getInverse()
	   
	   #Check to see if it has been already calculated:
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
	   #Get matrix stored in acche:
        data <- x$get()
	   
	   #Get inverse of that matrix
        i <- solve(data, ...)%*%data
        x$setInverse(i)
        i	   
}

