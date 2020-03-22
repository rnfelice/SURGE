##' Utility function to test whether there are any curves missing from specimens
#'
#' @param lm.tibble a tibble of landmark data
#' @return just used inside of \code{import_chkpt_data} really


check.curves <- function(lm.tibble){
  uniques <- lm.tibble %>% group_by(spec.id) %>% filter(class == "C") %>% summarise(Unique_Elements = n_distinct(id))
  max_curves <- max(uniques$Unique_Elements)
  too_few <- which(uniques$Unique_Elements != max_curves)
  if (length(too_few)==0){
    writeLines("You have no missing curves! Great job!")
  }
  else{
    for (i in 1:length(too_few)){
      name <- uniques$spec.id[too_few[i]]
      present <- pts_tibble %>% group_by(spec.id) %>% filter(class == "C") %>% filter(spec.id == name) %>% distinct(id) %>% pull(id) %>% as.vector()
      absent <- setdiff(as.character(1:max_curves), present)
      writeLines("WARNING!!!!!")
      writeLines(paste(name, "is missing curve number", absent))
    }
  }
}
