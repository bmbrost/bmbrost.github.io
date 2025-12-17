library(mgcv)
library(splines)

df <- 9  # degree of basis expansion
n <- 100
X <- bs(1:n,df=df,intercept=TRUE)  # basis expansion
dim(X)
matplot(X,type="l")

beta <- c(0,-0.005,0,0.01,0,-0.01,0,0.005,0)
mu <- X%*%beta
y <- rnorm(100,mu,sd=0.004)

beta_star <- sapply(beta,function(x) rnorm(100,x,sd=0.005))

png("media/gam_logo.png",width=3,height=1,units="in",res=600,bg=NA)
par(mar=c(0.1,0.1,0.1,0.1))
matplot(1:100,X%*%t(beta_star),col=rgb(11/255,107/255,195/255,0.1),type="l",lty=1,axes=FALSE,xlab="",ylab="")
lines(mu)
s <- sample(1:n,20)
points(s,y[s],col="white",pch=19)
points(s,y[s],bg=rgb(195/255,11/255,102/255,0.35),col=rgb(195/255,11/255,102/255,0.55),pch=21)
dev.off()

