# outliers vector----------------------------------------------------------
#' NA Completion (xts Vector)
#' 
#' NA  completion
#' 
#' @note Use \code{NA_compl_matrix} for matrix (whole data set). 
#' @param x data as a vector
#' @param trans_flag code containing the transformation flag, currently only 1 (replace with one-sided median of preceding observations)  and 2 (replace with local median (obs + or - 3 on each side, adjusted if at beginning or end)) supported
#' @param wind window for median calculation (one-sided) 
#' @details series containing NA values are completed with one-sided median (\code{wind} preceding obs) \cr use of two-sided values (3 preceding and 3 following) wouldn't be possible with real time values \cr (but might be useful for factor detection from past values, or real time with delay - for factor detection...)
#' @return  transformed vector
#' @seealso \code{NA_compl_matrix}
#' @export
#' @author Sorin T. Pascu
NA_compl  <- function(x,trans_flag=1, wind=5) {
  #   1 replace with one-sided median (5 preceding obs) ----> here as trans_flag = 1 
  ##   2 replace with local median (obs + or - 3 on each side)
  ##   3 replace with maximum value
  ##  4 replace with median value
  # if (is.null(repl)){repl <- x} #repl = NULL, taken out
  y  <- x  
  
  if(trans_flag == 1) { # replace with one-sided median (window = 5 preceding observations)
    # browser()
    for ( i in 1:dim(y)[1]){ # x and y supposed as type xts --> 2 dimensions 
      # in case one of the first 5 values is affected, the median of the remaining preceding values is computed, 
      # including the value itself (for the case of first value being affected) 
      
      # lag_range  <- ((i>wind)*wind + (i<=wind)*(i-1)):(i>wind)
      lag_range  <- ((i>wind)*wind + (i<=wind)*(i-1)):1
      #           avoid missing values from start instead of using na.rm=TRUE ...
      if (is.na(y[i])) { 
        # browser()
        y[i]  <- median(lag(y,lag_range)[i]) # compute median over wind=5 lagged values
        # take median(...y...), of transformed series, as S&W ( not of original ...(x)...)
      }   
    }
  }  else if (trans_flag == 2) { 
    # 2 replace with local median (obs + or - 3 on each side, adjusted if at beginning or end)
    for ( i in 1:dim(y)[1]){ 
      lag_range  <- (((dim(y)[1]-i)<wind)*(dim(y)[1]-i) + ((dim(y)[1]-i)>i & i >wind)*wind + (i<=wind)*(i-1)):1 # now need to check 3 parts / domains
      if (is.na(y[i])) { 
        # browser()
        y[i]  <- median(lag(y,c(-lag_range,lag_range))[i])
      }   
    }
  }  else if (trans_flag == 3) {
    # some code for other transformations
  } 
  
  return(y)
}

# outliers - matrix -------------------------------------------------------
#' NA Completion (xts Matrix)
#' 
#' NA Completion 
#' 
#' @param data data as a matrix
######' @param header ---  to be adjusted --- header containing a field "NA_Code": 1 --> variable is adjusted (otherwise not, e.g. for 0)
#' @param trans_flag code containing the transformation flag, currently only 1 supported
#' @param wind window for median calculation (one-sided, preceding) 
#' @details .. \cr ..
#' @return  transformed matrix
#' @seealso \code{outlier_adj}
#' @author Sorin T. Pascu
#' @export
# NA_compl_matrix  <- function(data, header, repl_matr = NULL, trans_flag=1, wind=5){
NA_compl_matrix  <- function(data, trans_flag=1, wind=5){
  transf_data  <- data # create empty object with same object type 
  transf_data[1:dim(transf_data)[1],1:dim(transf_data)[2]]  <- NA # 
  
  for (i in 1:dim(data)[2]) {
    # browser()
    # if(header["NA_Code",colnames(data)[i]] == 1 ){ ####### all are checked, independent of NA_Code
    transf_data[,i]  <- NA_compl(data[,i, drop=FALSE],trans_flag=trans_flag, wind=wind)   
    # } ####### all are checked
  }
  return(transf_data=transf_data)  
}
#test github2
