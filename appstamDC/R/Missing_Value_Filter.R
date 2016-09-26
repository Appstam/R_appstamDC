# summary.filter
#' summary.filter
#' 
#' summary.filter
#' 
#' @details Function for .spar file
summary.filter <- function(x) { 
  cat("\n","-------------------------","\n","-------------------------","\n","\n","Overview Missing values:", "\n","\n")
  prmatrix(x$overview)
  cat("\n","Description:", "\n", "0 denotes that a value is missing. 1 denotes that a value is available.","\n",
      "Row names (column left) show number of cases affected." ,"\n", "The last column shows how many values are missing for every single case." ,"\n", 
      "The last row depicts the absolute number of missing values per feature.")
  cat("\n","\n","-------------------------","\n", "\n","Percentage of missing values per feature:","\n","\n","\n")
  print(x$percentage)
  cat("\n","-------------------------","\n","-------------------------")
}

# summary.nomiss
#' summary.nomiss
#' 
#' summary.nomiss
#' 
#' @details Function for .spar file
summary.nomiss <- function(x) {
  cat("\n","The data set does not contain any missing values.")
}


NA_pattern <- function(x) 
{ # Copy of function md.pattern() from the mice package on CRAN.
  if (!(is.matrix(x) | is.data.frame(x))) 
    stop("Data should be a matrix or dataframe")
  if (ncol(x) < 2) 
    stop("Data should have at least two columns")
  if (is.data.frame(x)) 
    x <- data.matrix(x)
  n <- nrow(x)
  p <- ncol(x)
  mode(x) <- "single"
  r <- 1 * is.na(x)
  nmis <- as.integer(apply(r, 2, sum))
  names(nmis) <- dimnames(x)[[2]]
  mdp <- (r %*% (2^((1:ncol(x)) - 1))) + 1
  ro <- order(mdp)
  x <- matrix(x[ro, ], n, p)
  mdp <- mdp[ro]
  r <- matrix(r[ro, ], n, p)
  ro <- order(ro)
  mdpst <- as.integer(seq(along = mdp)[!duplicated(mdp)])
  mdp <- unique(mdp)
  npatt <- length(mdpst)
  r <- 1 - r
  r <- matrix(r[mdpst, ], npatt, p)
  if (npatt == 1) 
    tmp <- format(n)
  if (npatt > 1) 
    tmp <- format(c(mdpst[2:npatt], n + 1) - mdpst)
  dimnames(r) <- list(tmp, dimnames(x)[[2]])
  storage.mode(r) <- "integer"
  if (npatt > 1) 
    nmdp <- as.integer(c(mdpst[-1], n + 1) - mdpst)
  if (npatt == 1) 
    nmdp <- n
  co <- order(nmis)
  ro2 <- order(nmis.row <- p - as.integer(apply(r, 1, sum)))
  r <- rbind(r[ro2, co], nmis[co])
  r <- cbind(r, c(nmis.row[ro2], sum(nmis)))
  r
}



filtereddata <- function(InputDataFrame, FilteredColumn,OnlyFilteredCases){
  
  if(sum(is.na(InputDataFrame))==0) {
    
    modelv <- vector()
    class(modelv) <- "nomiss"
    cases <- TRUE}
  
  else{
    missing_overview <- NA_pattern(InputDataFrame)
    pMiss <- function(x){round(sum(is.na(x))/length(x)*100,digits=2)}
    percentage_missing <- sort(apply(InputDataFrame,2,pMiss))
    modelv <- list(overview= missing_overview,percentage=percentage_missing)
    class(modelv) <- "filter"
    
    if(length(FilteredColumn)==1){cases <- if (OnlyFilteredCases=="Rows with Missing Data") {is.na(InputDataFrame[,FilteredColumn])} else if (OnlyFilteredCases=="All") {
      TRUE} else {!is.na(InputDataFrame[,FilteredColumn])}}
    else{cases <- if (OnlyFilteredCases=="Rows with Missing Data") {apply(is.na(InputDataFrame[,FilteredColumn]),1,any)} else if (OnlyFilteredCases=="All") {
      TRUE} else  {apply(!is.na(InputDataFrame[,FilteredColumn]),1,all)}}
  }
  
  return(list(output=InputDataFrame[cases,],model=modelv))
}