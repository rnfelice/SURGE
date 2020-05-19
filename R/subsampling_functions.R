#subsampling code written by Leo Bitton-Divet

cursub.closer<-function(cur,req)
{
  mat<-as.matrix(dist(cur))
  DPO<-NULL
  for (j in 1:nrow(cur)-1)
  {
    a<-mat[j+1,j]
    DPO<-c(DPO,a)
  }
  DCO<-NULL
  for (j in 1:length(DPO))
  {
    DCO[j]<-sum(DPO[1:j])
  }
  DN<-sum(DPO)/(req+1)
  DCN<-DN*(1:req)
  clo<-NULL
  for (k in 1:length(DCN))
  {
    clo[k]  <- which.min(abs(DCO- DCN[k]))
  }
  clo<-clo+1
  cur[clo,]
}
## extension à une liste de courbes et de nombre de points requis
subsampl.closer<-function(matlm,curlist,required,fix)
{
  output<-matlm[fix,]
  for (i in 1:length(curlist))
  {
    cur<-matlm[curlist[[i]],]
    req<-required[i]
    out<-cursub.closer(cur,req)
    output<-rbind(output,out[1:length(out),])
  }
  output
}
## penser faire un check pour les doublons, la prise de point anatomique comme point de courbe... bref c'est un peu dangereux si on a pas nbptsinitial>>nptsrequis

############################################################################
##                    RESAMPLING AVEC INTEPOLATION                        ##
############################################################################

cursub.interpo<-function(cur,req)
{
  mat<-as.matrix(dist(cur))
  DPO<-NULL									#DPO= distance pt to pt originale
  for (j in 1:nrow(cur)-1)
  {
    a<-mat[j+1,j]
    DPO<-c(DPO,a)
  }
  DCO<-NULL									#DCO= DIstance cordale au point initial originale
  for (j in 1:length(DPO))
  {
    DCO[j]<-sum(DPO[1:j])
  }

  DN<-sum(DPO)/(req+1)								#Distance totale/nb de points = distance entre 2 points qui seront créés (+1 car pour n points requis, il y a n+1 intervalespuisqu'on ne compte pas les points initiaux et terminaux qui sont anatomiques.
  DCN<-DN*(1:(req))  								#DCN= distance entre le point initial et les nouveaux points
  proxima <- matrix(nrow=req,ncol=2)						#matrice contenant les points les plus proches
  for (k in 1:length(DCN))
  {
    first <- which.min(abs(DCO- DCN[k]))				#point le plus proche
    proxima[k,1] <- first
    second <- which.min(abs(DCO[-first]- DCN[k]))
    ifelse(first==second,second<-(second+1),second<-second)		#vu qu'on vire le pt le + proche du vecteur si first=second, second est en réalité le point first+1
    proxima[k,2] <- second#second point les plus proche
  }
  proxima<-proxima+1 								#car dist du 1er pt à lui meme=0 du coup il n'apparait pas dans les mesures de dist length =5 pour 6pt)
  proxima2 <- matrix(nrow=req,ncol=2)
  for (i in 1:req)
  {
    if(proxima[i,1]>proxima[i,2])
    {
      proxima2[i,1]<-proxima[i,2];proxima2[i,2]<-proxima[i,1]}
    else if (proxima[i,1]<proxima[i,2]){
      proxima2[i,1]<-proxima[i,1];proxima2[i,2]<-proxima[i,2]}
  }									#remise des points dans l'ordre d'index croissant (si le point le plus proche est d'un rowindex plus élevé que le second point le plus proche)
  VEC<-matrix( nrow=req, ncol = 3)						#VEC=vecteur du point proximal[n,1] au point proximal[n,2]
  for(l in 1:req)
  {
    VEC[l,]<-as.matrix(cur[proxima2[l,2],]-cur[proxima2[l,1],])
  }
  COMP<-NULL									#COMP=distance entre le point à créer et le point proximal[n,i]
  for(n in 1:req)
  {
    COMP[n]<-DCN[n]-DCO[proxima2[n,1]-1]				#car DCO fait p-1 point car dist cordale du premier point à lui meme=0
  }
  NOR<-NULL  									#Norme à appliquer au vecteur
  for(m in 1:req)
  {
    NOR[m]<-COMP[m]/(DPO[proxima2[m,1]])				####corrigee le 20-07-15
  }
  VECF<-VEC*NOR
  PTS<-cur[proxima2[,1],]+VECF
  PTS<-rbind(cur[1,],PTS,cur[dim(cur)[1],])
  PTS
}

## extension à une liste de courbes et de nombre de points requis
subsampl.inter<-function(matlm,curlist,required,fix)
{
  if (is.list(curlist)==F)
    print("curlist must be a list giving the curves(rowindex)")
  else
    if (is.vector(required)==F)
      print("required must be a vector giving the number of points required per curve")
  else
    if (length(curlist)!=length(required))
      print("curlist and required must be of same length")
  else

    output<-matlm[fix,]
  for (i in 1:length(curlist))
  {
    cur<-matlm[curlist[[i]],]
    req<-required[i]
    out<-cursub.interpo(cur,req)
    rownames(out)<-paste("curve",i,"-",(1:dim(out)[1]-1),sep="")
    output<-rbind(output,out[2:(dim(out)[1]-1),])
  }
  output
}


subsampl.inter2<-function(matlm,curlist,required,fix)
{
  if (is.list(curlist)==F)
    print("curlist must be a list giving the curves(rowindex)")
  else
    if (is.vector(required)==F)
      print("required must be a vector giving the number of points required per curve")
  else
    if (length(curlist)!=length(required))
      print("curlist and required must be of same length")
  else

    output<-matlm[fix,]
  for (i in 1:length(curlist))
  {

    cur<-matlm[curlist[[i]],]
    req<-required[i]
    if (any(matlm[curlist[[i]],]==9999)) {
      out <- matrix(9999, nrow = req+2,  ncol = 3)
    }
    else {
      out<-cursub.interpo(cur,req)
    }
    rownames(out)<-paste("curve",i,"-",(1:dim(out)[1]-1),sep="")
    output<-rbind(output,out[2:(dim(out)[1]-1),])
  }
  output
}
