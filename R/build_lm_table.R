##' Interactively Make Landmark Synonymy Table
#'
#' @param target_spec a p x 3 matrix of 3d landmark data for the template or specimen that has the correct landmarking scheme
#' @param original_spec a matrix of 3d landmarks representing the landmarking scheme to be changed
#' @param description a character vector giving names or descriptions for each landmark in the target template
#' @param sub_num a numerical vector giving numbers for each landmark in the target template, ideally per-curve or per-patch
#' @return a table
#'
#' @export
#' @examples
#' my_lm_table<-build_lm_table(target_spec = bird_lm_curves[,,1],
#' original_spec = croclms[,,1],
#' description = birdmoduledefs$description,
#' sub_num = birdmoduledefs$landmark_designation,
#' begin=1)

build_lm_table<-function(target_spec, original_spec, description, sub_num, begin=1){
  lm_synonyms<-tibble(ptnum = c(1:dim(target_spec)[1]), description=description, sub_num=sub_num, original_num =rep(0,dim(target_spec)[1]))
  open3d();mfrow3d(nc=1,nr=2)
  i=begin
  while (i<=dim(target_spec)[1]){
    rownames(target_spec) <- c(1:nrow(target_spec))
    n <- dim(target_spec)[1]
    index <- as.numeric(rownames(target_spec))
    clear3d();plot3d(target_spec,size=5,col="black",xlab="x",ylab="y",zlab="z",aspect=FALSE)
    text3d(target_spec, texts=index,cex=1,adj=c(2,1))
    points3d(target_spec[i,1],target_spec[i,2],target_spec[i,3],size=10,color="red",add=TRUE)
    next3d()
    writeLines(paste0("select landmark ", lm_synonyms$sub_num[i]," for ", lm_synonyms$description[i]))
    ids <- plot3d(original_spec,size=5,col="black",xlab="x",ylab="y",zlab="z",aspect=FALSE)
    text3d(original_spec, texts=index,cex=1,adj=c(2,1))
    keep <- selectpoints3d(ids["data"], value= FALSE, button = "right")[2]
    points3d(original_spec[keep,1],original_spec[keep,2],original_spec[keep,3],size=10,color="blue",add=TRUE)
    continue <- readline(paste0("You selected landmark ", keep, ". OK?\n", "(return=next lm | r=preselect this lm\nn = mark as NA | s=stop viewing)\n"))
    if (continue == "r") {
      i <- i-1
    }
    else if (continue =="s"){
      i <- dim(target_spec)[1]
    }
    else if (continue =="n"){
      lm_synonyms$original_num[i]<-NA
      #i <- i+1
    }
    else
      lm_synonyms$original_num[i]<-keep
    i <- i+1
    next3d()
  }
  return(lm_synonyms)
}
