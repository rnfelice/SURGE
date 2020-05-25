##' Utility function to test whether there are any curves missing from specimens
#'
#' @export
#' @param lm.tibble a tibble of landmark data
#' @return just used inside of \code{import_chkpt_data} really


check_curves <- function(lm.tibble,ncurves){
  uniques <- lm.tibble %>% group_by(spec.id) %>% filter(class == "C") %>% summarise(Unique_Elements = n_distinct(id))
  #max_curves <- max(uniques$Unique_Elements)
  too_few <- which(uniques$Unique_Elements < ncurves)
  if (length(too_few)==0){
    writeLines("You have no missing curves! Great job!")
  }
  else{
    for (i in 1:length(too_few)){
      name <- uniques$spec.id[too_few[i]]
      present <- lm.tibble %>% group_by(spec.id) %>% filter(class == "C") %>% filter(spec.id == name) %>% distinct(id) %>% pull(id) %>% as.vector()
      absent <- setdiff(as.character(1:ncurves), present)
      writeLines("WARNING!!!!!")
      writeLines(paste(name, "is missing curve number", absent))
    }
  }
  too_many <- which(uniques$Unique_Elements > ncurves)
  if (length(too_many)==0){
    writeLines("You have no extra curves! Great job!")
  }
  else{
    for (i in 1:length(too_many)){
      name <- uniques$spec.id[too_many[i]]
      present <- lm.tibble %>% group_by(spec.id) %>% filter(class == "C") %>% filter(spec.id == name) %>% distinct(id) %>% pull(id) %>% as.vector()
      absent <- setdiff(present,as.character(1:ncurves))
      writeLines("WARNING!!!!!")
      writeLines(paste(name, "has extra curve number", absent))
    }
  }

}
