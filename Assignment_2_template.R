#  Assignment 2
#  Introduce the <<- operator which can be used to assign 
#  a value to an object in an environment that is different 
#  from the current environment.
makeVector <- function(x = numeric()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    get <- function() x
    
    setmean <- function(mean) m <<- mean
    
    getmean <- function() m
    
    list(set = set, get = get,
         setmean = setmean,
         getmean = getmean)
}

cachemean <- function(x, ...) {
    
    m <- x$getmean()
    
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- mean(data, ...)
    x$setmean(m)
    m
}

#  source('Assignment_2.R')
#  z <-c(15,65,34,96,42,57)
#  zz <-makeVector(z)
# > class(zz)
# [1] "list"
# > mean(z)
# [1] 51.5
# > cachemean(zz)
# [1] 51.5
#