library(car)
library(stargazer)
library(MASS)
library(AER)
library(ggplot2)
library(nnet)
library(gridExtra)
library(reshape)
library(extrafont)

font_install('fontcm')
loadfonts()


#race
pew$white<-pew$PPETHM
pew$white<-recode(pew$white, "-2=NA")
pew$white<-recode(pew$white, "-1=NA")
pew$white<-recode(pew$white, "2=0")
pew$white<-recode(pew$white, "3=0")
pew$white<-recode(pew$white, "4=0")
pew$white<-recode(pew$white, "5=0")
pew$other<-pew$PPETHM
pew$other<-recode(pew$other,"-2=NA")
pew$other<-recode(pew$other,"-1=NA")
pew$other<-recode(pew$other,"1=0")
pew$other<-recode(pew$other,"2=0")
pew$other<-recode(pew$other,"3=1")
pew$other<-recode(pew$other,"4=0")
pew$other<-recode(pew$other,"5=1")
pew$hisp<-pew$PPETHM
pew$hisp<-recode(pew$hisp,"-2=NA")
pew$hisp<-recode(pew$hisp,"-1=NA")
pew$hisp<-recode(pew$hisp,"1=0")
pew$hisp<-recode(pew$hisp,"2=0")
pew$hisp<-recode(pew$hisp,"3=0")
pew$hisp<-recode(pew$hisp,"4=1")
pew$hisp<-recode(pew$hisp,"5=0")

#social net
pew$poorppl<-pew$Q9C 
pew$smallgov<-pew$Q4
pew$poorppl<-recode(pew$poorppl,"-1=NA")
pew$poorppl<-recode(pew$poorppl,"2=0")
pew$smallgov<-recode(pew$smallgov,"-1=NA")
pew$smallgov<-recode(pew$smallgov,"2=0")

#discrimination
pew$bdis<-pew$Q6_A
pew$bdis<-recode(pew$bdis,"-1=NA")
pew$hdis<-pew$Q6_B
pew$hdis<-recode(pew$hdis,"-1=NA")

#immigration
pew$immi<-pew$Q9A
pew$immi<-recode(pew$immi,"-1=NA")
pew$immi<-pew$immi-1

#gay policies
pew$jobpol<-pew$Q80_A
pew$jobpol<-recode(pew$jobpol,"-1=NA")
pew$jobpol<-5-pew$jobpol
pew$marpol<-pew$Q80_B
pew$marpol<-recode(pew$marpol,"-1=NA")
pew$marpol<-5-pew$marpol
pew$adoptpol<-pew$Q80_C
pew$adoptpol<-recode(pew$adoptpol,"-1=NA")
pew$adoptpol<-5-pew$adoptpol

#queer policies
pew$hivpol<-pew$Q80_D
pew$hivpol<-recode(pew$hivpol,"-1=NA")
pew$hivpol<-5-pew$hivpol
pew$youthpol<-pew$Q80_F
pew$youthpol<-recode(pew$youthpol,"-1=NA")
pew$youthpol<-5-pew$youthpol
pew$transpol<-pew$Q80_G
pew$transpol<-recode(pew$transpol,"-1=NA")
pew$transpol<-5-pew$transpol

#participation
pew$orgmem<-pew$Q83_A
pew$orgmem<-recode(pew$orgmem,"-1=NA")
pew$orgmem<-recode(pew$orgmem,"2=1")
pew$orgmem<-recode(pew$orgmem,"3=0")
pew$rally<-pew$Q83_D
pew$rally<-recode(pew$rally,"-1=NA")
pew$rally<-recode(pew$rally,"2=1")
pew$rally<-recode(pew$rally,"3=0")
pew$pride<-pew$Q83_E
pew$pride<-recode(pew$pride,"-1=NA")
pew$pride<-recode(pew$pride,"2=1")
pew$pride<-recode(pew$pride,"3=0")
pew$donate<-pew$Q83_F
pew$donate<-recode(pew$donate,"-1=NA")
pew$donate<-recode(pew$donate,"2=1")
pew$donate<-recode(pew$donate,"3=0")

#party
pew$rep<-pew$PARTY
pew$rep<-ifelse(pew$PARTY == 1,1,ifelse(pew$PARTY == 3 & pew$PARTYLN == 1,1,0 ))

#ideology
pew$IDEO<-recode(pew$IDEO, "-1=NA")

#controls
pew$INCOME<-recode(pew$INCOME,"-1=NA")
pew$PPEDUCAT<-recode(pew$PPEDUCAT,"-1=NA")
pew$PPEDUCAT<-recode(pew$PPEDUCAT,"-2=NA")
pew$ppagecat<-recode(pew$ppagecat,"99=NA")

#gender
pew$SEX<-recode(pew$SEX,"-1=NA")
pew$SEX<-recode(pew$SEX,"2=0")

#linked fate
pew$linkedlesbians<-pew$Q33_A
pew$linkedlesbians<-recode(pew$linkedlesbians,"-1=NA")
pew$linkedlesbians<-5-pew$linkedlesbians
pew$linkedgays<-pew$Q33_B
pew$linkedgays<-recode(pew$linkedgays,"-1=NA")
pew$linkedgays<-5-pew$linkedgays
pew$linkedbi<-pew$Q33_C
pew$linkedbi<-recode(pew$linkedbi,"-1=NA")
pew$linkedbi<-5-pew$linkedbi
pew$linkedtrans<-pew$Q33_D
pew$linkedtrans<-recode(pew$linkedtrans,"-1=NA")
pew$linkedtrans<-5-pew$linkedtrans
pew$linktot <- rowSums(pew[,c("linkedlesbians", "linkedgays","linkedbi","linkedtrans")], na.rm=TRUE)


#linked fate models
lesmod<-polr(as.factor(linkedlesbians)~white+rep+white*rep+INCOME+PPEDUCAT+ppagecat+SEX,data=pew,Hess=TRUE,model=TRUE)
gaymod<-polr(as.factor(linkedgays)~white+rep+white*rep+INCOME+PPEDUCAT+ppagecat+SEX,data=pew,Hess=TRUE,model=TRUE)
bimod<-polr(as.factor(linkedbi)~white+rep+white*rep+INCOME+PPEDUCAT+ppagecat+SEX,data=pew,Hess=TRUE,model=TRUE)
transmod<-polr(as.factor(linkedtrans)~white+rep+white*rep+INCOME+PPEDUCAT+ppagecat+SEX,data=pew,Hess=TRUE,model=TRUE)
totmod<-lm(linktot~white+rep+white*rep+INCOME+PPEDUCAT+ppagecat+SEX,data=pew)

totdat<-data.frame(white = rep(0:1,2),rep = rep(1:0,each = 2),INCOME=mean(pew$INCOME,na.rm=TRUE), PPEDUCAT=mean(pew$PPEDUCAT, na.rm=TRUE),ppagecat=mean(pew$ppagecat,na.rm=TRUE),SEX=mean(pew$SEX,na.rm = TRUE))
totpred<-predict(totmod,totdat,type="response",se.fit=TRUE)
totpreds<-data.frame(type = c("NW Rep", "White Rep", "NW Dem", "White Dem"),predicted = totpred$fit, UL = totpred$fit + totpred$se.fit, LL = totpred$fit - totpred$se.fit)

totp<-ggplot(totpreds, aes(type,predicted)) + 
  geom_point(size=1) +
  geom_pointrange(aes(ymin=LL,ymax=UL),size=1)+ 
  theme_bw()+ 
  theme(text = element_text(family="CM Roman"))+
  labs(y="Cumulative Linked Identity") + 
  theme(axis.text.x=element_text(size=14),axis.text.y=element_text(size=12),axis.title=element_text(size=15)) + 
  theme(axis.title.x = element_blank())
totp



#general social net (aka big gov)
socialnetmodel<-glm(smallgov~white+other+hisp+rep+INCOME+PPEDUCAT+ppagecat+SEX,data=pew,family=binomial)
summary(socialnetmodel)

snmod2<-glm(smallgov~white+rep+white*rep+INCOME+PPEDUCAT+ppagecat+SEX,data=pew,family=binomial)
summary(snmod2)

sndata = data.frame(white = rep(0:1,2), rep = rep(1:0,each =2), INCOME=mean(pew$INCOME,na.rm=TRUE), PPEDUCAT=mean(pew$PPEDUCAT, na.rm=TRUE),ppagecat=mean(pew$ppagecat,na.rm=TRUE),SEX=mean(pew$SEX,na.rm = TRUE))
save<-predict(snmod2, sndata, type="response", se.fit = TRUE)
preds<-data.frame(type = c("NW Rep", "White Rep", "NW Dem", "White Dem"),predicted = save$fit, UL = save$fit + save$se.fit, LL = save$fit - save$se.fit)

snp<-ggplot(preds, aes(type,predicted)) + 
  geom_point(size=1) +
  geom_pointrange(aes(ymin=LL,ymax=UL),size=1)+ 
  theme_bw()+ 
  theme(text = element_text(family="CM Roman"))+
  labs(y="Predicted Probabilities") + 
  theme(axis.text.x=element_text(size=14),axis.text.y=element_text(size=12),axis.title=element_text(size=15)) + 
  theme(axis.title.x = element_blank())
snp

#discrimination
dismodel<-polr(as.factor(bdis)~white+other+hisp+rep+INCOME+PPEDUCAT+ppagecat+SEX,data=pew,Hess=TRUE,model=TRUE)
summary(dismodel)
coeftest(dismodel)
stargazer(dismodel)

dismodel1<-polr(as.factor(bdis)~white+rep+white*rep+INCOME+PPEDUCAT+ppagecat+SEX,data=pew,Hess=TRUE,model=TRUE)
coeftest(dismodel1)

dishispmodel<-polr(as.factor(hdis)~white+other+hisp+rep+INCOME+PPEDUCAT+ppagecat+SEX,data=pew,Hess=TRUE,model=TRUE)
summary(dishispmodel)
coeftest(dishispmodel)

dishispmodel1<-polr(as.factor(hdis)~white+rep+white*rep+INCOME+PPEDUCAT+ppagecat+SEX,data=pew,Hess=TRUE,model=TRUE)
coeftest(dishispmodel1)

#gay policies
job2<-polr(as.factor(jobpol)~white+rep+white*rep+INCOME+PPEDUCAT+ppagecat+SEX,data=pew,Hess=TRUE,model=TRUE)
coeftest(job2)

mar2<-polr(as.factor(marpol)~white+rep+white*rep+INCOME+PPEDUCAT+ppagecat+SEX,data=pew,Hess=TRUE,model=TRUE)
coeftest(mar2)

adopt2<-polr(as.factor(adoptpol)~white+rep+white*rep+INCOME+PPEDUCAT+ppagecat+SEX,data=pew,Hess=TRUE,model=TRUE)
coeftest(adopt2)


#queer policies
hivpolsmod<-polr(as.factor(hivpol)~white+rep+white*rep+INCOME+PPEDUCAT+ppagecat+SEX,data=pew,Hess=TRUE,model=TRUE)
summary(hivpolsmod)
coeftest(hivpolsmod)

hivpolsmod1<-polr(as.factor(hivpol)~white+rep+white*rep+linkedtrans+INCOME+PPEDUCAT+ppagecat+SEX,data=pew,Hess=TRUE,model=TRUE)

youthpolsmod<-polr(as.factor(youthpol)~white+rep+white*rep+INCOME+PPEDUCAT+ppagecat+SEX,data=pew,Hess=TRUE,model=TRUE)
summary(youthpolsmod)
coeftest(youthpolsmod)

transpolsmod<-polr(as.factor(transpol)~white+rep+white*rep+INCOME+PPEDUCAT+ppagecat+SEX,data=pew,Hess=TRUE,model=TRUE)
summary(transpolsmod)
coeftest(transpolsmod)

#participation
orgmod<-glm(orgmem~white+rep+white*rep+INCOME+PPEDUCAT+ppagecat+SEX,data=pew,family=binomial)
summary(orgmod)

rallymod<-glm(rally~white+rep+white*rep+INCOME+PPEDUCAT+ppagecat+SEX,data=pew,family=binomial)
summary(rallymod)

pridemod<-glm(pride~white+rep+white*rep+INCOME+PPEDUCAT+ppagecat+SEX,data=pew,family=binomial)
summary(pridemod)

donatemod<-glm(donate~white+rep+white*rep+INCOME+PPEDUCAT+ppagecat+SEX,data=pew,family=binomial)
summary(donatemod)

#white money
whitemoney<-lm(INCOME~white,data=pew)
summary(whitemoney)

#immigrant model
immmod1<-glm(immi~white+rep+white*rep+INCOME+PPEDUCAT+ppagecat+SEX,data=pew,family=binomial)
summary(immmod1)

#party
repmod<-glm(rep~white+INCOME+PPEDUCAT+ppagecat+SEX,data=pew,family=binomial)
summary(repmod)
indmod<-glm(ind~white+INCOME+PPEDUCAT+ppagecat+SEX,data=pew,family=binomial)
summary(indmod)
