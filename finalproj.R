library(haven)
library(VGAM)
library(AER)
library(foreign)
library(ggplot2)
library(MASS)
library(Zelig)
library(nnet)
library(reshape2)
library(stargazer)
require(gridExtra)
library(cowplot)
library(grid)
library(psy)
library(reshape)
library(car)
lgbt <- read_sav("C:/Users/khinkso2/Downloads/pew2013lgbtpublicdatarelease.sav")

#recoding variables
lgbt$Q4<-recode(lgbt$Q4,"-1=NA")
lgbt$Q8<-recode(lgbt$Q8,"-1=NA")
lgbt$Q9A<-recode(lgbt$Q9A,"-1=NA")
lgbt$Q9C<-recode(lgbt$Q9C,"-1=NA")
lgbt$Q31<-recode(lgbt$Q31,"-1=NA")
lgbt$Q32<-recode(lgbt$Q32,"-1=NA")
lgbt$Q44<-recode(lgbt$Q44,"-1=NA")
lgbt$Q45<-recode(lgbt$Q45,"-1=NA")
lgbt$Q46<-recode(lgbt$Q46,"-1=NA")
lgbt$Q47<-recode(lgbt$Q47,"-1=NA")
lgbt$Q49<-recode(lgbt$Q49,"-1=NA")
lgbt$Q67<-recode(lgbt$Q67,"-1=NA")
lgbt$Q68<-recode(lgbt$Q68,"-1=NA")
lgbt$Q71<-recode(lgbt$Q71,"-1=NA")
lgbt$Q72<-recode(lgbt$Q72,"-1=NA")
lgbt$Q74<-recode(lgbt$Q74,"-1=NA")
lgbt$Q83_A<-recode(lgbt$Q83_A,"-1=NA")
lgbt$Q83_B<-recode(lgbt$Q83_B,"-1=NA")
lgbt$Q83_C<-recode(lgbt$Q83_C,"-1=NA")
lgbt$Q83_D<-recode(lgbt$Q83_D,"-1=NA")
lgbt$Q83_E<-recode(lgbt$Q83_E,"-1=NA")
lgbt$Q83_F<-recode(lgbt$Q83_F,"-1=NA")
lgbt$Q85a<-recode(lgbt$Q85a,"-1=NA")
lgbt$Q98<-recode(lgbt$Q98,"-1=NA")
lgbt$Q99<-recode(lgbt$Q99,"-1=NA")
lgbt$ATTEND<-recode(lgbt$ATTEND,"-1=NA")
lgbt$attend1<-7-lgbt$ATTEND
lgbt$IDEO<-recode(lgbt$IDEO,"-1=NA")
lgbt$PARTY<-recode(lgbt$PARTY,"-1=NA")
lgbt$PARTY<-recode(lgbt$PARTY,"4=NA")
lgbt$idimport<-6-lgbt$Q31
lgbt$q9a<-3-lgbt$Q9A
lgbt$q83a<-4-lgbt$Q83_A
lgbt$q83b<-4-lgbt$Q83_B
lgbt$q83c<-4-lgbt$Q83_C
lgbt$q83d<-4-lgbt$Q83_D
lgbt$q83e<-4-lgbt$Q83_E
lgbt$q83f<-4-lgbt$Q83_F

attach(lgbt)


#id importance
hist(Q31,main="Overall Identity Importance", xlab="",breaks=rep(1:5,each=2)+c(-.4,.4),cex.lab=1.5,xaxt="n")
box()
axis(1,at=seq(1,5,1),labels=c("Extremely","Very","Somewhat", "Not Too", "Not At All"))

#come out to mom
table(Q44)
#come out to dad
table(Q45)
#measure of just being out in general
table(Q49)
#out online 
table(Q85a)
#out at work
table(Q99)
#alpha
cronbach(Y)
#factor analysis
Y<-cbind(Q44,Q45,Q49,Q85a,Q99)
outfactor<-factanal(na.omit(Y),factors=1,scores="regression")
outfactor
lambda<-unclass(outfactor$loadings) # getting factor loadings for each item.
psi<-diag(outfactor$uniquenesses) # uniquenesses
sigma<-var(Y) #variabnce
gamma<-t(lambda)%*%solve(psi)%*%lambda
beta<-t(solve(1+gamma)%*%t(lambda)%*%solve(psi))
lgbt$outfactor1<-scale(Y, TRUE, TRUE)%*%beta
lgbt$outfactora<-as.numeric(lgbt$outfactor1)
remove(lambda,psi,sigma,gamma,beta)

#Govt size (483 smaller, 702 bigger)
hist(Q4)
#gun ownership (about 2 to 1)
hist(Q8)
#immigration (about 2 to 1) recode this
hist(q9a)
#poor people (about 2 to 1 poor people have it hard)
hist(Q9C)
#alpha
cronbach(Y1)
#factor analysis
Y1<-cbind(Q4,Q8,q9a,Q9C)
policyfactor<-factanal(na.omit(Y1),factors=1,scores="regression")
policyfactor
lambda<-unclass(policyfactor$loadings) # getting factor loadings for each item.
psi<-diag(policyfactor$uniquenesses) # uniquenesses
sigma<-var(Y1) #variabnce
gamma<-t(lambda)%*%solve(psi)%*%lambda
beta<-t(solve(1+gamma)%*%t(lambda)%*%solve(psi))
lgbt$policyfactor1<-scale(Y1, TRUE, TRUE)%*%beta
lgbt$policyfactora<-as.numeric(lgbt$policyfactor1)
remove(lambda,psi,sigma,gamma,beta)



#conflict with religious id
par(mfrow=c(1,2))
hist(Q67,main="Religious Conflict", xlab="",breaks=rep(0:1,each=2)+c(-.4,.4),cex.lab=1.5,xaxt="n")
box()
axis(1,at=seq(0,1,1),labels=c("No Conflict","Conflict"))
#religious attendance - low
hist(ATTEND,main="Religious Attendance", xlab="",breaks=rep(1:6,each=2)+c(-.4,.4),cex.lab=1.5,xaxt="n")
box()
axis(1,at=seq(1,6,1),labels=c("Never","Seldom","Few Times/Year", "1-2/Month","1/Week",">1/Week"),cex.axis=.6)


#social acceptance where you live (normal-esque)
hist(Q71)
#lgbt friends
hist(Q74)
#accepting workplace
hist(Q98)
#alpha
cronbach(Y2)
#factor analysis
Y2<-cbind(Q71,Q74,Q98)
isolationfactor<-factanal(na.omit(Y2),factors=1,scores="regression")
lambda<-unclass(isolationfactor$loadings) # getting factor loadings for each item.
psi<-diag(isolationfactor$uniquenesses) # uniquenesses
sigma<-var(Y2) #variabnce
gamma<-t(lambda)%*%solve(psi)%*%lambda
beta<-t(solve(1+gamma)%*%t(lambda)%*%solve(psi))
lgbt$isolationfactor1<-scale(Y2, TRUE, TRUE)%*%beta
lgbt$isolationfactora<-as.numeric(lgbt$isolationfactor1)
remove(lambda,psi,sigma,gamma,beta)



#lgbt specific political participation
#member of lgbt organization 
hist(Q83_A)
#bought product bc company supported lgbt 
table(Q83_B)
#boycotted bc didn't support 
table(Q83_C)
#rally/march 
table(Q83_D)
#pride
table(Q83_E)
#donate money to politics
table(Q83_F)
#alpha
cronbach(Y3)
#factor analysis
Y3<-cbind(q83a,q83b,q83c,q83d,q83e,q83f)
partfactor<-factanal(na.omit(Y3),factors=1,scores="regression")
lambda<-unclass(partfactor$loadings) # getting factor loadings for each item.
psi<-diag(partfactor$uniquenesses) # uniquenesses
sigma<-var(Y3) #variabnce
gamma<-t(lambda)%*%solve(psi)%*%lambda
beta<-t(solve(1+gamma)%*%t(lambda)%*%solve(psi))
lgbt$partfactor1<-scale(Y3, TRUE, TRUE)%*%beta
lgbt$partfactora<-as.numeric(lgbt$partfactor1)
remove(lambda,psi,sigma,gamma,beta)

#control variables: income, edu, gender, race
hist(INCOME)
hist(PPEDUCAT)
hist(PPETHM)
hist(SEX)

#dependent variables
#party id
par(mfrow=c(1,2))
hist(PARTY,main="Party Identification", xlab="",breaks=rep(1:3,each=2)+c(-.4,.4),cex.lab=1.5,xaxt="n",freq=TRUE)
box()
axis(1,at=seq(1,3,1),labels=c("Republican","Democrat","Independent"))
#ideology
hist(IDEO,main="Ideology", xlab="",breaks=rep(1:5,each=2)+c(-.4,.4),cex.lab=1.5,xaxt="n",freq=TRUE)
box()
axis(1,at=seq(1,5,1),labels=c("V. Con","","Moderate","","V. Liberal"))

#actual model - multinomial, party
model1<-multinom(PARTY~idimport+outfactora+policyfactora+Q67+attend1+isolationfactora+partfactora+INCOME+PPEDUCAT+SEX,lgbt)
summary(model1)
model1a<-vglm(PARTY~idimport+outfactora+policyfactora+Q67+attend1+isolationfactora+partfactora+INCOME+PPEDUCAT+SEX,multinomial,data=lgbt)
summary(model1a)
AIC(model1a)

#actual model - ordered logit, ideo
model2<-polr(as.factor(IDEO)~idimport+outfactora+policyfactora+Q67+attend1+isolationfactora+partfactora+INCOME+PPEDUCAT+SEX,data=lgbt,Hess=TRUE,model=TRUE)
summary(model2)
stargazer(model2)
AIC(model2)
brant(model2)

#brant test had issues with education, lets drop it. doesn't seem to be a problem. 
model2a<-polr(as.factor(IDEO)~idimport+outfactora+policyfactora+Q67+attend1+isolationfactora+partfactora+INCOME+SEX,data=lgbt,Hess=TRUE,model=TRUE)
summary(model2a)
library(ordinal)
mod1<-clm(as.factor(IDEO)~idimport+outfactora+policyfactora+Q67+attend1+isolationfactora+partfactora+INCOME+SEX+PPEDUCAT,data=lgbt)
mod2<-clm(as.factor(IDEO)~idimport+outfactora+policyfactora+Q67+attend1+isolationfactora+partfactora+INCOME+SEX,data=lgbt)
anova(mod1,mod2)

#does getting rid of insignficant control variables change shit? nope, AIC barely changes and nothing changes signiicance or magnitude in any important way
model1b<-multinom(PARTY~idimport+outfactora+policyfactora+Q67+attend1+isolationfactora+partfactora+INCOME,lgbt)
summary(model1b)
model2b<-polr(as.factor(IDEO)~idimport+outfactora+policyfactora+Q67+attend1+isolationfactora+partfactora,data=lgbt,Hess=TRUE,model=TRUE)
summary(model2b)

#let's interpret this shit - odds ratio
exp(coef(model1))
exp(coef(model1a))
exp(coef(model2))
cintervals<-confint(model2)

#predicted probabilities - ideology; by closeted and conflict
#no conflict
predict1<-cbind(outfactora=seq(-2,3,length=100),
               idimport=3.02,policyfactora=0.003515,Q67=0,attend1=2.336,isolationfactora=-0.0328, partfactora=-0.00136, INCOME=mean(INCOME),PPEDUCAT=mean(PPEDUCAT),SEX=mean(SEX))
predictprob1<-predict(model2,predict1,type='prob')

#plot
closeted<-seq(-1,3,length=100)

plot(c(-1,3),c(0,1),type='n',
     xlab="Closeted",
     ylab="Predicted Probabilities",
     main="",
     cex.lab=1.25, cex.main=1.5)
lines(closeted,predictprob1[1:100,1],lty=1,lwd=3)
lines(closeted,predictprob1[1:100,2],lty=2,lwd=3)
lines(closeted,predictprob1[1:100,3],lty=3,lwd=3)
lines(closeted,predictprob1[1:100,4],lty=4,lwd=3)
lines(closeted,predictprob1[1:100,5],lty=5,lwd=3)
legend(1,1,cex=1,c('V. Conservative','Conservative','Moderate','Liberal','V. Liberal'),
       lty=1:5)

plot(c(-1,3),c(0,1),type='n',
     xlab="Closeted",
     ylab="Predicted Probabilities",
     main="",
     cex.lab=1.25, cex.main=1.5)
lines(closeted,predictprob1[1:100,2],lty=2,lwd=3)
lines(closeted,predictprob1[1:100,4],lty=4,lwd=3)
legend(1,1,cex=1,c('Conservative','Liberal'),
       lty=2,4)

#standard errors
ndraws <- 500
coefficients<-model2$coeff
cutpoints <- model2$zeta
coefficients<-c(coefficients,cutpoints)
covariancematrix <- solve(model2$Hessian)
betadraw <- mvrnorm(ndraws,coefficients,covariancematrix)
