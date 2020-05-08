##' Create curve info object
#'
#' @param curvedata a dataframe or tibble with columns "lm1", "lm2" and "ptswanted".
#' @param n_fixed a number indicating how many landmarks are fixed anatomical landmarks
#' @return output contains the necessary information to subsample curves, and the identity of each curve for sliding
#' \item{Curves}{a list of curves with the landmarks that begin and end each curve}
#' \item{Curve.in}{the same list of curves without the landmarks that begin and end each}
#' \item{Sliding.LMs}{an integer vector of all the points that are sliding semilandmarks on curves}
#' \item{Fixed}{an integer vector of all the points that are fixed landmarks that are not allowed to slide}
#' @examples
#' my_curvedata <- read_csv("curvedatafile.csv")
#' my_curves <- create_curve_info(curvedata = my_curvedata, n_fixed = 32)

create_curve_info<-function(curvedata, n_fixed){
  #define the curves using the 'curvedata' object
  if (class(n_fixed) != "numeric"){
    stop("n_fixed must be a number")
  }
  c.names <-  c("lm1","lm2","ptswanted")
  if (length(setdiff(c.names, colnames(curvedata))) != 0)
  {stop("curvedata must contain columns lm1 lm2 and ptswanted ")
  }
  fixed <- c(1:n_fixed)
  subsampled.curve<-sapply(paste("SC",c(1:nrow(curvedata)),sep=""),function(x) NULL)
  subsampled.curve[[1]]<-c(curvedata$lm1[1],((length(fixed)+1):(length(fixed)+curvedata$ptswanted[1])),curvedata$lm2[1])
  if(nrow(curvedata)!=1){
  for (i in 2:length(subsampled.curve)){
    subsampled.curve[[i]]<-c(curvedata$lm1[i],((max(unlist(subsampled.curve))+1):(max(unlist(subsampled.curve))+curvedata$ptswanted[i])),curvedata$lm2[i])
  }
}
  subsampled.curve.in<-subsampled.curve
  for(i in 1:length(subsampled.curve.in)){
    subsampled.curve.in[[i]]<-tail(head(subsampled.curve[[i]],-1),-1)

  }


  slidings.sub<-c((max(fixed)+1):max(unlist(subsampled.curve)))

  list("Curves" = subsampled.curve, "Curve.in" = subsampled.curve.in, "Sliding.LMs" = slidings.sub, "Fixed" = c(1:n_fixed))
}
