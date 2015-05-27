library(faraway)

y<-c(320,14,80,36)
particle<-gl(2,1,4,labels=c("no","yes"))
quality<-gl(2,2,labels=c("good","bad"))
wafer<-data.frame(y,particle,quality)
wafer

#Poisson model

ov<-xtabs(y~quality+particle,wafer) 
ov

mod1<-glm(y~particle+quality,wafer,family=poisson)
mod1
summary(mod1)

mod1.1<-summary(glm(y~quality,wafer,family=poisson))
mod1.2<-summary(glm(y~particle,wafer,family=poisson))
drop1(mod1,test="Chi")

mod1.1

mod1.2

#Read discussion at the bottom of page 70 - top of page 71.

#Why do we conclude that with deviance of 54.03 and degrees of freedom 1 the model does not fit?

### Multinomial model
#Note the relationship between Poisson distribution and multinomial distribution on page 73.

###Binomial model
#Note how we formulate the binomial model problem: 
  #create 2 groups and check if they have the same probability of success.

(m<-matrix(y,nrow=2))

modb<-glm(m~1,family=binomial)
summary(modb)

deviance(modb)

#Null model means that predictor (particles) is not involved. If the model fits well predictor is irrelevant.

#What exactly we fit by modb?

#Check the fields of the object modb.

names(modb)

modb$y

#How was the output modb$y created from m or ov, both of which are grouped versions of wafer?

wafer
m
ov

#What link is used in modb?
#Explain the values of modb$linear.predictors, fitted.values.

modb$y

modb$linear.predictors

modb$coeff

modb$fitted.values

#What does fitting of glm() tell us about the effect of particle?

###Visualization of tables

data(haireye)
haireye

(ct<-xtabs(y~hair+eye,haireye))

dotchart(ct)

mosaicplot(ct,color=TRUE,main=NULL,las=1)


##################################################################################
### week 9 workshop ###
##################################################################################

library(lme4)

#Simulate data and analyze them using lmer() from lme4 for the following situation:
  
#Segmentation of the market for a product shows that probability of success of marketing strategy depends on the age group and gender.
#Probability of success measured for 3 age groups: 20-30, 30-40 and 40-50 and within each age group the probability is measured by gender.
#There were 100 participants of each gender in every age group.
#The results are presented in the following table

Marketing.Probabilities.Data<-data.frame(Age.20.30=c(.7,.55,.1,.2),Age.30.40=c(.5,.4,.1,.2),Age.40.50=c(.3,.2,.07,.09))
rownames(Marketing.Probabilities.Data)<-c("F.mean","M.mean","F.sd","M.sd")

Marketing.Probabilities.Data

#we need 3 columns - age group, gender, probabilities
library(knitr)
kable(Marketing.Probabilities.Data,format="markdown")

#Overall error of observation ?? has normal distribution with zero mean and ????=.02.
#Simulate data frame consistent with these observations and reproduce the analysis.

Marketing.Probabilities <- data.frame(age=(factor(c(rep("Age.20.30",200),rep("Age.30.40",200),rep("Age.40.50",200)))),
           gender=(factor(rep(c(rep("Male",100),rep("Female",100)),3))),
           prob=c(rnorm(100,mean=.55,sd=.2),
                  rnorm(100,mean=.7,sd=.1),
                  rnorm(100,mean=.4,sd=.2),
                  rnorm(100,mean=.5,sd=.1),
                  rnorm(100,mean=.2,sd=.09),
                  rnorm(100,mean=.3,sd=.07)))

head(Marketing.Probabilities)

attach(Marketing.Probabilities)

Mean.Sd.Table <- aggregate.data.frame(prob, by = list(age,gender), FUN=function(x) c(mn =round(mean(x),3), stdev=round(sd(x),3)) )
# colnames(Mean.Sd.Table) <- c("age","gender","prob_mean","prob_sd")
# kable(Mean.Sd.Table)

Model.by.Age <- lm(prob ~ age)
Model.by.Age.Gender <- lm(prob ~ age + gender)

anova(Model.by.Age)
anova(Model.by.Age.Gender)

m1 <- lmer(prob ~ (1|age))
summary(m1)
m2 <- lmer(prob ~ (1|age) + (1|gender))
summary(m2)

m3 <- lmer(prob ~ (1|age/gender))
summary(m3)

detach(Marketing.Probabilities)




#######################################################################3
# week 10 workshop#

library(animation)

# set some options first
oopt = ani.options(interval = .5, nmax = 25)
# Create Transition Probabilities Matix
MC.Matrix<-rbind(From.Pos1=c(To.Pos1=0,To.Pos2=0,To.Pos3=0,To.Pos4=0),
                From.Pos2=c(To.Pos1=0,To.Pos2=0,To.Pos3=0,To.Pos4=0),
                From.Pos3=c(To.Pos1=0,To.Pos2=0,To.Pos3=0,To.Pos4=0),
                From.Pos4=c(To.Pos1=0,To.Pos2=0,To.Pos3=0,To.Pos4=0))
Current.State<-2
From.To<-rep(Current.State,2)

# use a loop to create images one by one

run.ani <- function(){
    
  my.Data<-as.matrix(t(c(0,2)))
  
  for (i in 1:ani.options("nmax")) {
   From.To[1]<-Current.State
   #    From.To<-c(1,1)
   From.To[2]<-sample(1:4,1,prob=MC.Matrix[From.To[1],])
   Current.State<-From.To[2]
  
   #wait.next<-rexp(1,lambda)
   my.Data<-rbind(my.Data,c(my.Data[length(my.Data[,1]),1]+1,From.To[2]))
   plot(my.Data,type="s",xlim=c(1,25),ylim=c(0,4),xlab="Time",ylab="State",lwd=3)
   abline(h=1,col="red")
   abline(h=4,col="red")
   ani.pause() 
   ## pause for a while ('interval')
  }
  # restore the options
  ani.options(oopt)
  
}

library(knitr)

### Matrix 1
MC.Matrix<-data.frame(rbind(From.Pos1=c(To.Pos1=1,To.Pos2=0,To.Pos3=0,To.Pos4=0),
                            From.Pos2=c(To.Pos1=.03,To.Pos2=0,To.Pos3=.97,To.Pos4=0),
                            From.Pos3=c(To.Pos1=0,To.Pos2=.97,To.Pos3=0,To.Pos4=.03),
                            From.Pos4=c(To.Pos1=0,To.Pos2=0,To.Pos3=0,To.Pos4=1)))
kable(MC.Matrix)

run.ani()

### Matrix 2
MC.Matrix<-data.frame(rbind(From.Pos1=c(To.Pos1=1,To.Pos2=0,To.Pos3=0,To.Pos4=0),
                            From.Pos2=c(To.Pos1=.03,To.Pos2=0,To.Pos3=.97,To.Pos4=0),
                            From.Pos3=c(To.Pos1=0,To.Pos2=.97,To.Pos3=0,To.Pos4=.03),
                            From.Pos4=c(To.Pos1=0,To.Pos2=0,To.Pos3=0,To.Pos4=1)))
kable(MC.Matrix)

run.ani()

### Matrix 3
MC.Matrix<-data.frame(rbind(From.Pos1=c(To.Pos1=.01,To.Pos2=.99,To.Pos3=0,To.Pos4=0),
                            From.Pos2=c(To.Pos1=.3,To.Pos2=0,To.Pos3=.7,To.Pos4=0),
                            From.Pos3=c(To.Pos1=0,To.Pos2=.7,To.Pos3=0,To.Pos4=.3),
                            From.Pos4=c(To.Pos1=0,To.Pos2=0,To.Pos3=.99,To.Pos4=.01)))
kable(MC.Matrix)

run.ani()

#################
library(depmixS4)

data(speed)
head(speed)

plot(speed$rt,type="l",lty=1)

#select first series of trials
sp1 <- data.frame(speed[1:168,])
names(sp1) <- c("RT", "ACC","Pacc")

hist(speed$rt)

#Fit the one component model. First, create the mix model using mix(). Then fit it.
m1 <- mix(RT~1,nstates=1, data=sp1)
fm1 <- fit(m1)
fm1

summary(fm1)
#Since 1-state model is not really an HMM the summary only gives us the mean and the standard deviation of the variable RT.
#Fit a two-component model for RT.

set.seed(1)
# Create the object with 2 states
m2 <- mix(RT~1,nstates=2, data=sp1,
          respstart=c(rnorm(1,5),1,rnorm(1,6),1))
fm2 <- fit(m2,emcontrol=em.control(rand=F))
fm2
summary(fm2)
names(summary(fm2))

#Parameter respstart sets randomly selected initial guesses. 
#Note that AIC and BIC are better for the 2-state model. 
#The summary gives the means and standard deviations of the 
      #two normal distributions as the states of the model.

#Plot the mix.

library(nor1mix)

Two.State.mix<-norMix(mu=c(5.475281,6.313695),sigma=c(0.1255941,0.3192222),w=c(0.3315286,0.6684714))
plot(Two.State.mix)

#Fit a 1-state model for both variables: multinomial for accuracy and Gaussian for response time; 
      #for both Pacc is the predictor.

m1p <- depmix(list(ACC~Pacc,RT~Pacc), nstates=1,
              data=sp1, family=list(multinomial(),gaussian()))
fm1p <- fit(m1p)
fm1p
summary(fm1p)

#Fit a 2-state model with intercept only and Gaussian and multinomial distributions 
      #for RT and ACC correspondingly.
set.seed(1)
m2 <- depmix(list(RT~1,ACC~1),nstates=2, data=sp1,
             family=list(gaussian(),multinomial()))
fm2 <- fit(m2)
fm2
summary(fm2)

#Fit a model with intercepts only for Gaussian RT and multinomial ACC, 
      #making transition probabilities dependent on Pacc

set.seed(1)
m2a <- depmix(list(RT~1,ACC~1),nstates=2, data=sp1,
              family=list(gaussian(),multinomial()),
              transition=~Pacc)
fm2a <- fit(m2a)
fm2a

#Fit a 3-state model with intercepts only for Gaussian RT and multinomial ACC

set.seed(1)
m3 <- depmix(list(RT~1,ACC~1),nstates=3, data=sp1,
             family=list(gaussian(),multinomial()))
fm3 <- fit(m3)
fm3
summary(fm3)

#Of all the models the two 2-state models show better fits.
#And the champion is the 2-state HMM with Pacc as covariate

#######################
### SELF ORGANIZED MAPS

library(kohonen)

AssignmentData<-
  read.csv(file="C:/Users/Patrick/Documents/R/UChicago/Linear_NonLinear/regressionassignmentdata2014.csv",
           header=TRUE,sep=",")
AssignmentData[1:10,2:8]

AssignmentData.som <- som(as.matrix(AssignmentData[,2:8]), grid = somgrid(2,2, "hexagonal"))
plot(AssignmentData.som)

names(AssignmentData.som)
AssignmentData.som$codes

#It is better to scale all variables.

AssignmentData.som <- som(scale(as.matrix(AssignmentData[,2:8])), grid = somgrid(2,2, "hexagonal"))
par(mfrow=c(2,2))
plot(AssignmentData.som,"codes",codeRendering ="lines")
plot(AssignmentData.som,"counts")
plot(AssignmentData.som,"changes")
plot(AssignmentData.som,"quality")

#USE SOM WITH RESPONSE TIME DATA FROM ABOVE

speed.som<-som(as.matrix(speed[,c(1)]),grid=somgrid(1,2,"hexagonal"))
speed.som$codes

par(mfrow=c(2,2))
plot(speed.som,"counts")
plot(speed.som,"changes")
plot(speed.som,"quality")
plot(speed[,1],type="l")
abline(h=speed.som$codes)


###############################################################################################
#READ THE FOLLOWING ARTICLE

#Modeling of cognitive processes

#Gilles Dutilh, Eric-Jan Wagenmakers, Ingmar Visser, Han L. J. van der Maas, 
      #A Phase Transition Model for the Speed-Accuracy Trade-Off in Response Time Experiments, 
      #Cognitive Science 35 (2011) 211–250, 2010 Cognitive Science Society, Inc.

#Ingmar Visser, Seven things to remember about hidden Markov models: 
      #A tutorial on Markovian models for time series, Journal of Mathematical Psychology, 55, 
      #(2011), 403–415