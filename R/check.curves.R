##' Utility function to test whether there are any curves missing from specimens
#'
#' @export
#' @param lm.tibble a tibble of landmark data
#' @return just used inside of \code{import_chkpt_data} really


check_curves <- function(lm.tibble,ncurves,filenames){
  uniques <- lm.tibble %>% group_by(spec.id) %>% filter(class == "C")
  missinglist<-c()
  extraslist<-c()
  for (i in 1:length(unique(lm.tibble$spec.id))){
  x <- uniques %>% filter(spec.id == filenames[i]) %>% pull(id)
  missings1 <- setdiff(as.character(c(1:ncurves)),x)
  extras1 <- setdiff(x, as.character(c(1:ncurves)))
  if(length(missings1!=0)){
    writeLines(paste("WARNING!!", filenames[i], "is missing curve number", missings1,"\n"))
    missinglist<-c(missinglist,filenames[i])}
  if(length(extras1!=0)){
    writeLines(paste("WARNING!!",filenames[i], "has an extra curve number", extras1,"\n"))
    extraslist<-c(extraslist,filenames[i])}
  }
  if(length(missinglist)==0){
    writeLines("You have no missing curves! Great job!")
  }
  if(length(extraslist)==0){
    writeLines("You have no extra curves! Great job!")
  }
  }
