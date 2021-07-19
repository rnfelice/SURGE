##' Import PTS files from Checkpoint
#'
#' @param ptslist a character vector of file names indicating the pts files to be imported
#' @param curveinfo the output of the function \code{create_curve_info}.
#' @param subsample a logical value specifying whether or not to subsample the semilandmark curves to equal lengths
#' @return If \code{subsample = TRUE}, returns a 3D array containing imported XYZ coordinate data, compatible with geomorph analyses. If \code{subsample = FALSE}, returns a tibble of coordinate data.
#'
#' @export
#' @examples
#' my_curvedata <- read_csv("curvedatafile.csv") #read data
#' my_curves <- create_curve_info(curvedata = my_curvedata, n_fixed = 32)
#' import_chkpt_data(ptslist = filenames, curveinfo = my_curves, subsampl = TRUE)


import_chkpt_data<-function(ptslist, curveinfo, subsampl=TRUE, verbose=FALSE){
  #######Right here i have to chcek that the inputs have the required stuff!

  filenames <- ptslist
  ntaxa <- length(filenames)
  pts_tibble <- tibble()

  for(i in 1:length(ptslist)) {
    specimen.tmp <- as_tibble(read.table(file=ptslist[i],skip=2,header=F,sep="")) #import a single specimen
    specimen.tmp <- specimen.tmp %>% mutate(., V1 = as.character(V1)) #convert the first row, the lm names, to characters instead of factors
    specimen.tmp <- specimen.tmp %>% mutate(.,spec.id=filenames[i]) #add a column with the specimen name
    pts_tibble<-bind_rows(pts_tibble,specimen.tmp) #paste it to the end of the tibble with the rest of the specimens
  }
  #this will give a warning message but its nothing to worry about
  pts_tibble <- suppressWarnings(pts_tibble %>% separate(., V1, into = c("class", "id","sub_lm"), remove = FALSE))

  pts_tibble <- pts_tibble %>% mutate(., id = as.factor(id)) %>%
    rename(., index = V1, X = V2, Y = V3, Z = V4) #rename the coordinate data columns
  check_curves(pts_tibble, ncurves=length(curveinfo$Curves),filenames=filenames)
  if(subsampl==FALSE){
    return(pts_tibble)
  }
  else{
    #make a list of how many sliding semilandmarks on curves there are
    curvepoints <- length(curveinfo$Sliding.LMs)

    #convert the tibble to a 3D array compatable with geomorph
    pts_tibble_tmp <- pts_tibble%>%filter(.,class=="S")%>%group_by(spec.id)%>%dplyr::select(.,X,Y,Z)%>%nest()%>%purrr::transpose()
    fixed_counts<-list()
    for(i in 1:length(pts_tibble_tmp)){
      fixed_counts[i]<-dim(pts_tibble_tmp[[i]]$data)[1]
    }
    wronglist<-filenames[which(unlist(fixed_counts)!=length(curveinfo$Fixed))]
    if(length(wronglist)!=0){
     stop(paste0("Error: ", wronglist," has the wrong number of fixed points (",unlist(fixed_counts)[which(unlist(fixed_counts)!=length(curveinfo$Fixed))],")\n"))
      }

    ptsarray_tmp <- array(dim=c(length(curveinfo$Fixed),3,ntaxa))

    for(i in 1:length(pts_tibble_tmp)){
      ptsarray_tmp[,,i] <- as.matrix(dplyr::select(pts_tibble_tmp[[i]]$data, c(X,Y,Z)))
    }


    #make an empty array with the correct number of landmarks and specimens
    newpts <- array(data = NA, dim = c(length(curveinfo$Fixed),3,ntaxa))
    #give it dimension names based on your specimen list
    dimnames(newpts)[3] <- list(substr(filenames,1,(nchar(filenames)-4)))
    #fill in the fixed landmarks
    newpts[curveinfo$Fixed,,] <- ptsarray_tmp

    for (which.curve in 1:length(curveinfo$Curve.in)){
      this.curve <- array(data=NA, dim=c(length(curveinfo$Curve.in[[which.curve]]),3,ntaxa))
      for (which.spec in 1:length(filenames)){
        orig.curve <- pts_tibble %>% filter(.,spec.id==filenames[which.spec])%>%filter(., class=="C")%>%filter(., id==which.curve) %>% dplyr::select(., X,Y,Z)
        orig.curve.anchors <- pts_tibble %>% filter(.,spec.id==filenames[which.spec])%>%slice(c(curveinfo$Curves[[which.curve]][1],last(curveinfo$Curves[[which.curve]]))) %>% dplyr::select(., X,Y,Z)
        orig.curve <- rbind(orig.curve.anchors[1,],orig.curve,orig.curve.anchors[2,])
        new.curve <- cursub.interpo(orig.curve, length(curveinfo$Curve.in[[which.curve]]))
        #this bit checks if you had ANY 9999s in this curve before subsampling.
        if(9999 %in% (orig.curve %>% pull(X))){
          #If TRUE, fill those values back in with 9999, just in case the code accidentilly subsampled a missing curve, making nonsense variables
          new.curve[c(2:dim(new.curve)[1]-1),]<-9999
        }
        this.curve[,,which.spec] <- as.matrix(new.curve)[2:(dim(new.curve)[1]-1),]
        if(verbose==TRUE){
        writeLines(text=paste0("Specimen ",which.spec," curve ", which.curve, " OK"))
        }
      }
      newpts <- abind::abind(newpts, this.curve, along=1)
    }
    return(newpts)
  }

}
