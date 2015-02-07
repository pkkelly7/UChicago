library(animation)

saveHTML({
 ## set some options first
 oopt = ani.options(interval = 1, nmax = 25)
 ## use a loop to create images one by one
 lambda<-1.1
 my.Data<-as.matrix(t(c(0,0)))
 dim(my.Data)
 set.seed(1840385)
 for (i in 1:ani.options("nmax")) {
   wait.next<-rexp(1,lambda)
   my.Data<-rbind(my.Data,c(my.Data[length(my.Data[,1]),1]+wait.next,my.Data[length(my.Data[,1]),2]+1))
   plot(my.Data,type="s",xlim=c(0,30),ylim=c(0,30),xlab="Time",ylab="Count")
   ani.pause(wait.next) 
   ## pause for a while ('interval')
 }
 ## restore the options
 ani.options(oopt)
},img.name = "pois_plot", title = "Demonstration of Poisson Process",
description = c("Counting process: with random time intervals (exponentially distributed)",
               "we observe events and count their number"),htmlfile="poissonprocess.html")

############################################################################################################

#Example from slide 8 in lecture notes --- Poisson Dist
ppois(9,7.5,lower.tail = FALSE)
ppois(10,15,lower.tail = FALSE)

#Gamma Distribution
Variable.X<-seq(from=0,to=10,by=.1)
Gamma.Parameters<-cbind(Shape=c(1,2,3,5,9),Scale=c(2,2,2,1,.5))
Gamma.Curves<-apply(Gamma.Parameters,1,function(z) dgamma(x=Variable.X,shape=z[1],scale=z[2]))
matplot(Variable.X,Gamma.Curves,type="l",lty=1,col=c("red","green","blue","purple","gold"),lwd=3,ylab="Gamma Densities")

#Effect of distribution of the Poisson intensity on the probabilities of counts
#Note that the shape of the count probabilities is influenced by the shape of the gamma distribution of the intensity.
par(mfrow=c(1,3))
Variable.X.Gamma<-seq(from=0,to=3,by=.1)
Variable.X.Poisson<-seq(from=0,to=50,by=1)
Alpha1<-1
Beta1<-1/2
plot(Variable.X.Gamma,dgamma(Variable.X.Gamma,shape=Alpha1,scale=Beta1),type="l",col="black",lwd=3,xlab="Variable X",ylab="Gamma Density")
plot(Variable.X.Poisson,dnbinom(Variable.X.Poisson,size=Alpha1,prob=Beta1/(Beta1+1)),type="h",col="black",lwd=3,xlab="Variable X",ylab="Count Probabilities")
plot(Variable.X.Poisson,dpois(Variable.X.Poisson,Alpha1*Beta1),type="h",col="black",lwd=3,xlab="Variable X",ylab="Poisson Probabilities")

Alpha2<-2
Beta2<-1/5
plot(Variable.X.Gamma,dgamma(Variable.X.Gamma,shape=Alpha2,scale=Beta2),type="l",col="black",lwd=3,xlab="Variable X",ylab="Gamma Density")

plot(Variable.X.Poisson,dnbinom(Variable.X.Poisson,size=Alpha2,prob=Beta2/(Beta2+1)),type="h",col="black",lwd=3,xlab="Variable X",ylab="Count Probabilities")

plot(Variable.X.Poisson,dpois(Variable.X.Poisson,Alpha2*Beta2),type="h",col="black",lwd=3,xlab="Variable X",ylab="Poisson Probabilities")

Alpha3<-8
Beta3<-1/4
plot(Variable.X.Gamma,dgamma(Variable.X.Gamma,shape=Alpha3,scale=Beta3),type="l",col="black",lwd=3,xlab="Variable X",ylab="Gamma Density")

plot(Variable.X.Poisson,dnbinom(Variable.X.Poisson,size=Alpha3,prob=Beta3/(Beta3+1)),type="h",col="black",lwd=3,xlab="Variable X",ylab="Count Probabilities")

plot(Variable.X.Poisson,dpois(Variable.X.Poisson,Alpha3*Beta3),type="h",col="black",lwd=3,xlab="Variable X",ylab="Poisson Probabilities")

par(mfrow=c(1,1))
#Overdispersion
Poisson.Sample<-rpois(500,.25)
Poisson.Sample.Variance<-var(Poisson.Sample)
Gamma.Shape<- 1
Gamma.Scale<-1/4
Gamma.Mean<-Gamma.Shape*Gamma.Scale
NBinom.Probability<-Gamma.Shape*Gamma.Scale/(Gamma.Scale+1)
Negative.Binom.Theoretical.Mean<-Gamma.Shape*(1/NBinom.Probability-1)
Negative.Binom.Theoretical.Variance<-Gamma.Shape*(1-NBinom.Probability)/NBinom.Probability^2

NBinom.Sample<-rnbinom(500,size=Gamma.Shape,prob=NBinom.Probability)
Negative.Binom.Sample.Variance<-var(NBinom.Sample)
c(Poisson.Theoretical.Variance=.25,
  Poisson.Sample.Variance=Poisson.Sample.Variance,
  Negative.Binom.Theoretical.Variance=Negative.Binom.Theoretical.Variance,
  Negative.Binom.Sample.Variance=Negative.Binom.Sample.Variance)
