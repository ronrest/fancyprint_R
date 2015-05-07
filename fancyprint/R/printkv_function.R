#' strkv
#' 
#' Returns a key value pair as a string. You can specify that the value be 
#' positioned at a certain minimum column number, which can be used to line up 
#' multiple strkv() function calls. 
#' You can also specity how the gap between the key and the value should be 
#' filled up, aswell as the separator. 
#' 
#' @param key the content of the key 
#' @param val content of the value
#' @param sep the character used to separate a key from a value. 
#'        defaults to ": "
#' @param fill integer. The minimum number of columns to fill up before the 
#'        separator is displayed. Use this to line values up. 
#' @param round used for setting the number of decimal places to show (if val 
#'        was a numeric type). 
#'        If round is FALSE (Default), then no rounding occurs. 
#'        If round is an integer, then val is rounded to that many decimal 
#'        places
#' @param vas (boolean) Vectors As Strings. If a vector is fed as the val 
#'        argument, should it convert the entire vector into a string? This will 
#'        print all the values of the vector as a string if TRUE is used. 
#'        FALSE = print a key value pair for each element of the vector. 
#'        See the examples section to see how this is used. 
#'        (DEFAULT = TRUE)  
#'        
#' @keywords fancyprint, ascii, print
#' @examples
#' strkv("name", "Joe", sep="= ", fill=10, fill_char=".") 
#' strkv("age", 34, sep="= ", fill=10, fill_char=".") 
#' strkv("age", 56) 
#' 
#' strkv("rank", c(1,2,3,4))
#' [1] "rank: (1,2,3,4)"
#' 
#' strkv("rank", c(1,2,3,4), vas=FALSE)
#' [1] "rank: 1" "rank: 2" "rank: 3" "rank: 4"
#' 
#' @export

strkv <- function(key, val, sep=": ", fill=0, fill_char=" ", round=FALSE, 
                    vas=TRUE){
    # TODO: BUG: round = TRUE leads to rounding to 1 decimal, should be to 
    #            nearest int
    # TODO: check that the inputs are of the correct data type.
    # TODO: consider looking at c-style formatting to make it more efficient, 
    #       esp when generating the gap.
    # TODO: have an option max.vec to specify an upper limit to the number of 
    #       elements in a vector to print out before it show ... dots. 
    
    #-------------------------------------------------------------------------
    #                                 Fill with the necessary amount of spaces
    #-------------------------------------------------------------------------
    filler = ""
    length_key = nchar(key)
    gap = fill - length_key
    if (gap > 0){
        for (i in 1:gap) 
            filler = paste(filler, fill_char, sep="")
    }
    key = paste(key, filler, sep="")
    
    #-------------------------------------------------------------------------
    #                                                           Value ROunding
    #-------------------------------------------------------------------------
    if (round != FALSE){
        #TODO: make sure the value is of type numeric.
        val = round(val, digits=round)
    }
    
    #-------------------------------------------------------------------------
    #                                                     Print out the Result
    #-------------------------------------------------------------------------
    if (vas){
        val = paste(val, collapse=", ")
        val = paste("(", val, ")", sep="")
        return(paste(key, val, sep=sep))
    } else {
        return(paste(key, val, sep=sep))
    }
    
}



#' printkv
#' 
#' Print a key value pair. You can specify that the value be positioned at a 
#' certain minimum column number, which can be used to line up multiple 
#' prinkv() function calls. 
#' You can also specity how the gap between the key and the value should be 
#' filled up, aswell as the separator. 
#' 
#' @param key the content of the key 
#' @param val content of the value
#' @param sep the character used to separate a key from a value. 
#'        defaults to ": "
#' @param fill integer. The minimum number of columns to fill up before the 
#'        separator is displayed. Use this to line values up. 
#' @param round used for setting the number of decimal places to show (if val 
#'        was a numeric type). 
#'        If round is FALSE (Default), then no rounding occurs. 
#'        If round is an integer, then val is rounded to that many decimal 
#'        places
#' @param vas (boolean) Vectors As Strings. If a vector is fed as the val 
#'        argument, should it convert the entire vector into a string? This will 
#'        print all the values of the vector as a string if TRUE is used. 
#'        FALSE = print a key value pair for each element of the vector. 
#'        See the examples section to see how this is used. 
#'        (DEFAULT = TRUE)  
#'        
#' @keywords fancyprint, ascii, print
#' @examples
#' printkv("name", "Joe", sep="= ", fill=10, fill_char=".") 
#' printkv("age", 34, sep="= ", fill=10, fill_char=".") 
#' printkv("age", 56) 
#' 
#' printkv("rank", c(1,2,3,4))
#' [1] "rank: (1,2,3,4)"
#' 
#' printkv("rank", c(1,2,3,4), vas=FALSE)
#' [1] "rank: 1" "rank: 2" "rank: 3" "rank: 4"
#' 
#' @export
printkv <- function(key, val, sep=": ", fill=0, fill_char=" ", round=FALSE, 
                    vas=TRUE){
    # print out the result of strkv()
    print(strkv(key, val=val, sep=sep, fill=fill, fill_char=fill_char, 
                round=round, vas=vas))
}
