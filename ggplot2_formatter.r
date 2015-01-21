
require(scales)

# ---------------------------------------------------------------------------------------------
# Formatting functions for ggplot  graph axis
# ---------------------------------------------------------------------------------------------

#' Human Numbers: Format numbers so they're legible for humans
#' Use this in ggplot for labels where you might use the comma or percent functions from the 
#' Scales package.
#' 
#' Checks whether numbers are positive or negative. 
#' Allows up to 1 significant figure
#' sapply used for element-wise application of the humanity function as a vector may include
#' numbers where billions, millions or thousands are appropriate.
#'
#' @return a character vector the same length as the input vector
#' @param x a numeric vector to format, smbl a symbol you'd like to prefix your numbers by
#' @examples
#' human_numbers(c(1000000 , 1500000, 10000000000))
#' human_numbers(c(1.200000e+05, -2.154660e+05, 2.387790e+05, 4.343500e+04 ,5.648675e+12), "$")
#' ggplot2 + scale_y_continuous(labels = human_numbers)
#' ggplot2 + scale_x_continuous(labels = human_numbers)

human_numbers <- function(x, smbl = ""){
    humanity <- function(y){             
        
        if (!is.na(y)){
        
            b <- round_any(y / 1000000000, 0.1)
            m <- round_any(y / 1000000, 0.1)
            k <- round_any(y / 1000, 0.1)
            
            if ( y >= 0 ){ 
                y_is_positive <- ""
            } else {
                y_is_positive <- "-"
            }
                    
            if  ( m < 1){
                paste (y_is_positive, smbl,  k , "k", sep = "")
            } else if (b < 1){
                paste (y_is_positive, smbl, m ,"m", sep = "")
            } else {
                paste (y_is_positive, smbl,  comma(b), "b", sep = "")    
            }
        }
    }
    
   sapply(x,humanity)
}
