##' Create curve info object
#'
#' @param curvedata a dataframe or tibble with columns "lm1", "lm2" and "ptswanted".
#' @param n_fixed a number indicating how many landmarks are fixed anatomical landmarks
#' @return The sum of \code{x} and \code{y}.
#' @examples
#' my_curvedata <- read_csv("curvedatafile.csv")
#' my_curves <- create_curve_info(curvedata = my_curvedata, n_fixed = 32)

create_curve_info<-function(curvedata, n_fixed){
  #define the curves using the 'curvedata' object
  fixed <- c(1:n_fixed)
  subsampled.curve<-sapply(paste("SC",c(1:nrow(curvedata)),sep=""),function(x) NULL)
  subsampled.curve[[1]]<-c(curvedata$lm1[1],((length(fixed)+1):(length(fixed)+curvedata$ptswanted[1])),curvedata$lm2[1])
  for (i in 2:length(subsampled.curve)){
    subsampled.curve[[i]]<-c(curvedata$lm1[i],((max(unlist(subsampled.curve))+1):(max(unlist(subsampled.curve))+curvedata$ptswanted[i])),curvedata$lm2[i])
  }

  subsampled.curve.in<-subsampled.curve
  for(i in 1:length(subsampled.curve.in)){
    subsampled.curve.in[[i]]<-tail(head(subsampled.curve[[i]],-1),-1)

  }


  slidings.sub<-c((max(fixed)+1):max(unlist(subsampled.curve)))

  list("Curves" = subsampled.curve, "Curve.in" = subsampled.curve.in, "Sliding.LMs" = slidings.sub, "Fixed" = c(1:n_fixed))
}
