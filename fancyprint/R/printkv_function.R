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
#'        
#' @keywords fancyprint, ascii, print
#' @examples
#' printkv("age", 34, sep="= ", fill=10, fill_char=".") 
#' printkv("age", 56) 
#' @export

printkv <- function(key, val, sep=": ", fill=0, fill_char=" ", round=FALSE){
    # TODO: check that the inputs are of the correct data type.
    # TODO: consider looking at c-style formatting to make it more efficient, 
    #       esp when generating the gap.
    # TODO: Have a parameter "round", so that if the value argument is a numeric
    #       then it should round to that number of decimal places. 
    # 
    
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
    print(paste(key, val, sep=sep))
}
