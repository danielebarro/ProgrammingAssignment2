#The function 'makeCacheMatrix' has only one attribute which is a matrix given in input.
#This function can catch the inverse of the matrix given in input. 
makeCacheMatrix = function( x = matrix() ) {
    s = NULL #Initialize the solution to the inverse of the matrix to be a NULL value.
      
    set = function(y){ #Defines a function with name 'set'.
      x <<- y #Set the Matrix 'x' to a new Matrix 'y'.
      s <<- NULL #reset the value of the inverse of the matrix equal to NULL.
    }
        
    get = function() x #Assigne to 'get' a function that returns the Matrix 'x'
    setinverse = function(inverse) s <<- inverse #Assigne to 'setinverse' a function that set the inverse 's' of the matrix.
    getinverse = function() s #Return the inverse 's. of the matrix.
    
   #Returns a special vector containing the functions just defined above.
   list(set = set, #gives the name 'set' to the function set() defined above.
         get = get, #gives the name 'get' to the function get() defined above.
         setinverse=setinverse, #gives the name 'setinverse' to the function setinverse() defined above.
         getinverse=getinverse) #gives the name 'getinverse' to the function getinverse() defined above.
}


# Compute the inverse of the special matrix returned by "makeCacheMatrix".
#If the inverse has already been calculated (and the matrix has not
# changed), then the "cachesolve" retrieves the inverse from the cache.
cacheSolve <- function(x, ...) {
          s = x$getinverse() #Assigne to 's' the solution of the stored inverted matrix.
                if (!is.null(s)){ #Condition to check if any inverted matrix has been stored. 
                         message("getting cached data") #Worning message that displayes only if the inverse matrix has been stored.
                         return(s) 
                        }
          data = x$get() #Assigne to 'data' the the function 'get' so that the variable 'data' now containes the matrix give by the user.
          s = solve(data,...) #Assigne to 's' the solution to the inverse of the matrix given by the user.
          x$setinverse(s) #Call the function setinverse and store the invertse matrix.
          s #Print the inverted matrix.
        }
