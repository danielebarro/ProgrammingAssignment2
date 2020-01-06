## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix = function( x = matrix() ) {
    s = NULL #Initialize the solution to the inverse of the matrix to be a NULL value
      
    set = function(y){ #Defines a function with name 'set' 
      x <<- y #Set the Matrix, x, to a new Matrix, y,
      s <<- NULL #reset the value of the inverse of the matrix equal to NULL
    }
        
    get = function() x #Assigne to 'get' a function that returns the Matrix 'x'
    setinverse = function(inverse) s <<- inverse #Assigne to 'setinverse' a function that set the inverse, s, to inverse
    getinverse = function() s #Return the inverse, s, of the matrix. If the inverse function has 
                              #been manually setted than it returnes the setted inverse matrix
    
   #Returns a special vector containing the functions just defined
   list(set = set, #gives the name 'set' to the function set() defined above
         get = get, #gives the name 'get' to the function get() defined above
         setinverse=setinverse, #gives the name 'setinverse' to the function setinverse() defined above
         getinverse=getinverse) #gives the name 'getinverse' to the function getinverse() defined above
}


 ## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
          s = x$getinverse() #Assigne to, is the solution of the stored inverted matrix
                if (!is.null(s)){ #Condition to check if the any inverted matrix has been stored 
                         message("getting cached data") #Worning message that displayes only if the inverse matrix has been stored
                         return(s) #Returns the stored matrix only if it is not NULL
                        }
          data = x$get() #Assigne to 'data' the the function 'get' so that the variable 'data' now containes the matrix give by the user
          s = solve(data,...) #Assigne to 's' the solution to the inverse of the matrix given in input
          x$setinverse(s) #Call the function setinverse and store the invertse matrix
          s #Print the inverted matrix
        }
