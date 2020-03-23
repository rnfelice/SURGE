##' A version of the checkLM function from Morpho that plots specimens by module identity
#'
#' @param lm.tibble a tibble of landmark data
#' @return The sum of \code{x} and \code{y}.
#' @examples
#' color.palette <- c( "#6a3d9a","dimgrey","#fb9a99",  "gold", "#009E73",  "#D55E00", "#CC79A7", "cyan2",  "#e31a1c", "#0072B2", "#b2df8a", "#E69F00",  "whitesmoke" ,  "deeppink",   "#a6cee3",   "#F0E442","blue","red","brown", "black")
#' modulecolors1<-as.factor(module_defs$posthoc.jug.sep)
#' levels(modulecolors1)<-color.palette
#' check.modules(patched,path="./ply/",suffix=".ply",pt.size=.5,render="s",module.colors = modulecolors1,begin=1)


check.modules <- function(dat.array, path=NULL, prefix="", suffix=".ply", col="white", pt.size=NULL, alpha=1, begin=1, render=c("w","s"), point=c("s","p"), add=FALSE,meshlist=NULL, Rdata=FALSE, module.colors=NULL, text.lm=FALSE)
{
  k <- NULL
  marked <- NULL
  j <- 1
  if (!Rdata)
    load <- file2mesh
  outid <- NULL
  point <- point[1]
  ## set point/sphere sizes
  radius <- pt.size
  if (is.null(radius)) {
    if (point == "s")
      radius <- (cSize(dat.array[,,1])/sqrt(nrow(dat.array[,,1])))*(1/30)
    else
      radius <- 10
  }
  size <- radius
  render <- render[1]
  arr <- FALSE
  point <- point[1]
  if (point == "s") {
    rendpoint <- spheres3d
  } else if (point == "p") {
    rendpoint <- points3d
  } else {
    stop("argument \"point\" must be \"s\" for spheres or \"p\" for points")
  }
  dimDat <- dim(dat.array)
  if (length(dimDat) == 3) {
    n <- dim(dat.array)[3]
    name <- dimnames(dat.array)[[3]]
    arr <- TRUE
  } else if (is.list(dat.array)) {
    n <- length(dat.array)
    name <- names(dat.array)
  } else {
    stop("data must be 3-dimensional array or a list")
  }
  i <- begin
  if (render=="w") {
    back <- front <- "lines"
    rend <- wire3d
  } else {
    back <- front <- "filled"
  }
  if (!add || rgl.cur()==0)
    open3d()
  meshnames <-  paste(path,prefix,name,suffix,sep="")
  while (i <= n) {
    rgl.bringtotop()
    tmp.name <- meshnames[i]
    if (arr)
      landmarks <- dat.array[,,i]
    else
      landmarks <- dat.array[[i]]
    if (is.null(module.colors)) {
      outid <- rendpoint(landmarks,radius=radius, size=size)
      if (text.lm)
        outid <- c(outid, text3d(landmarks, texts=paste(1:dim(landmarks)[1], sep=""), cex=1, adj=c(1,1.5)))

      if (!is.null(meshlist)) {
        tmpmesh <- meshlist[[i]]
      } else if (!is.null(path)) {
        if (!Rdata) {
          tmpmesh <- file2mesh(tmp.name,readcol=TRUE)
        } else {
          input <- load(tmp.name)
          tmp.name <- gsub(path,"",tmp.name)
          tmpmesh <- get(input)
        }
      }
      if (!is.null(meshlist) || !is.null(path)) {

        outid <- c(outid,shade3d(tmpmesh,col=col,alpha=alpha,back=back,front=front))
        rm(tmpmesh)
        if (Rdata)
          rm(list=input)
        gc()

      }
    } else {
      outid <- rendpoint(landmarks,radius=radius, size=size, col= module.colors)
      if (text.lm)
        outid <- c(outid, text3d(landmarks, texts=paste(1:dim(landmarks)[1], sep=""), cex=1, adj=c(1,1.5)))

      if (!is.null(meshlist)) {
        tmpmesh <- meshlist[[i]]
      } else if (!is.null(path)) {
        if (!Rdata) {
          tmpmesh <- file2mesh(tmp.name,readcol=TRUE)
        } else {
          input <- load(tmp.name)
          tmp.name <- gsub(path,"",tmp.name)
          tmpmesh <- get(input)
        }
      }
      if (!is.null(meshlist) || !is.null(path)) {

        outid <- c(outid,shade3d(tmpmesh,col=col,alpha=alpha,back=back,front=front))
        rm(tmpmesh)
        if (Rdata)
          rm(list=input)
        gc()

      }
    }


    answer <- readline(paste("viewing #",i,"(return=next | p=previous | m=mark current | s=stop viewing)\n"))
    if (answer == "m") {
      marked[j] <- i
      j <- j+1
    } else if (answer == "s") {
      i <- n+1
    } else if (answer == "p") {
      i <- i-1
    } else
      i <- i+1
    rgl.pop(id=outid)
  }
  invisible(marked)
}
