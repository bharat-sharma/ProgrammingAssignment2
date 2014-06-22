## function to create a vector
## this function's inputs is a matrix "X"
## this function also uses a environment variable "m" to store & set values beyond this function's lexical scope

## input is "x" a matrix & the output is a list comrising four values.
makeVector <- function(x = matrix())
{
        ##m is the environment variable that is used to store/set values beyond the function's lexical scope.
        m <- NULL
        
        ## first sub function "set" 
        set <- function(y)
        {
                ## set x to input
                x <<- y
                
                ## reset m to NULL
                m <<- NULL
        }
        
        ## second function "get" fetches the value for "x" & returns it
        get <- function()
        {
                x       
        }
        
        ## third function "setmean" finds the value of environment variable "m" to "mean" 
        ## returns nothing.
        setmean <- function(mean)
        {
                m <<- mean
        }
        
        ## fourth function "getmean" fetches & returns the value of the environment variable "m"
        getmean <- function()
        {
                m
        }
        
        ## list is the returned value after setting calling all the four functions above.
        list(
                        set=set,
                        get=get,
                        setmean=setmean,
                        getmean=getmean
                )
}

## this function "cachemean" returns the mean of the input vector "x"
cachemean <- function(x, ...)
{
        ## this statement calls the getmean function on "x" & assigns the output to "m"
        m <- x$getmean()
        
        ## check if "m" is null
        if(!is.null(m))
        {
                ## if "m" is not null (meaning that the vector's mean is already cached),
                ## then display the message showing that value is being fetched from cache.
                message("getting cached data")
                
                ## after fetching the value from cache, return this value & exit the function.
                return(m)
        }
        
        ## if "m" is null, meaning that there was no cached value
        ## then first, call the get function on "x" & store returned values in "data"
        data <- x$get()
        
        ## now call the mean function on "data" & store the output in "m"
        m <- mean(data, ...)
        
        ## now set the mean of "x" with the new value of "m"
        ## before exiting the function, cache the calculated mean of the vector.
        ## that is store calculated mean of the vector "x" in environment.
        x$setmean(m)
        
        ## return the calculated mean to the calling function.
        m
}
