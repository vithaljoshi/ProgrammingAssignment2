## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#This function is used to make a cache matrix

makeCacheMatrix <- function(x = matrix()) {
    
    m<-NULL # sets the value of m to NULL
    
    set<-function(y){
        x<<-y
        m<<-NULL
    }
    
    get<-function() x
    setmatrix<-function(solve) m<<- solve
    getmatrix<-function() m
    list(set=set, get=get,setmatrix=setmatrix,getmatrix=getmatrix)

}


## Write a short comment describing this function
##This function returns the inverse from the cache when the matrix is identical and the input hasn't changed
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m<-x$getmatrix()
        
        #check to see if there exists a cache
        if(!is.null(m)){
            # check to see if the matrix hasn't changed
            if(identical(x$setmatrix(),x$getmatrix()) {
                return(m)
            }
            message("getting cached matrix")
            return(m)
        }
        matrix<-x$get()
        m<-solve(matrix, ...)
        x$setmatrix(m)
        m
}
