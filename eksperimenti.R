source("pomozne_funkcije.R")
source("bitonic_TSP_algoritem.R")


# 6 tock, v pravokotniku [0,10]x[0,10]
seznam_eks1 <- generiraj_tocke(0,10,0,10,6)

B_eks1 <- najdi_razdaljo(seznam_eks1)$dolzine #matrika B
C_eks1 <- najdi_razdaljo(seznam_eks1)$predniki #matrika C

cikel_eks1 <- izpisi_cikel(C_eks1, seznam_eks1)

narisi(seznam_eks1) #slika zgolj tock

narisi_cikel(seznam_eks1) #cikel

izmeri_cas <- function(n){ #funkcija za cas izvajanja funkcije najdi_razdaljo()
  casi <- c()
  for (x in 3:n){
    sez1 <- generiraj_tocke(0,(x+5),0,(x+5),x)
    start.time <- Sys.time()
    najdi_razdaljo(sez1)
    end.time <- Sys.time()
    time.taken <- end.time - start.time
    casi <- append(casi, as.numeric(time.taken))
  }
  return(casi)
}

casi1 <- izmeri_cas(30)[2:length(izmeri_cas(30))]

#kvadratna funkcija, ki se tockam najbolje prilega
linear_model2 <- lm(y~poly(x,2,raw=TRUE), data=data.frame(x=4:30,y=casi1))

#graf casa izvajanja
plot(4:30, casi1, xlab = "Število točk", ylab = "Čas", 
     main = "Čas izvajanja programa")
lines(4:30, predict(linear_model2, data.frame(x=4:30)), col='red')



