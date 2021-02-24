library(fBasics)
da <- read.table("d-ibm3dx7008.txt", header=TRUE)

ibm <- da[,2]
sibm <- ibm*100

basicStats(sibm)

mean(sibm)
var(sibm)
sqrt(var(sibm))
skewness(sibm)
kurtosis(sibm)

s1 <- skewness(sibm)
t1 <- s1/sqrt(6/9845)
t1

pv <- 2*(1-pnorm(t1))
pv

libm <- log(ibm+1)*100
t.test(libm)

normalTest(libm, method="jb")