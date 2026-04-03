Fonction_echantillon<-function(n,delta){
  moyennes<-matrix(c(-delta,-delta,delta,-delta,-delta,delta,delta,delta),nrow = 4,ncol = 2,byrow = TRUE)
  n_e<-4
  ech<-list()
  for (i in 1:n_e) {
    ech[[i]]<-data.frame(x1=rnorm(n,mean=moyennes[i,1],sd=1),
                         x2=rnorm(n,mean=moyennes[i,2],sd=1),
                         P=i)
  }
  echantillon<-rbind(ech[[1]],ech[[2]],
                     ech[[3]],ech[[4]])
  echantillon$P <- factor(echantillon$P)
  return(echantillon)
}

echantillons_El = Fonction_echantillon(5,3)

K_max=20
res_CAH<-agnes(echantillons_El, metric = "euclidiean", method="ward")
cutree_res_CAH<-cutree(res_CAH,k=1:K_max)
plot(res_CAH, which.plots = 2)

install.packages("mclust")
library(mclust)
z<-sapply(1:7, function(k) adjustedRandIndex(echantillons_El$P,cutree_res_CAH[,k]))
aricode
plot(z)