library(car)
library(MASS)
library(AER)
library(ggplot2)
library(nnet)
library(gridExtra)
library(extrafont)
library(cowplot)

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

#party
pew$rep<-pew$PARTY
pew$rep<-ifelse(pew$PARTY == 1,1,ifelse(pew$PARTY == 3 & pew$PARTYLN == 1,1,0 ))

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

#general social net (aka big gov)
socialnetmodel<-glm(smallgov~white+other+hisp+rep+INCOME+PPEDUCAT+ppagecat+SEX,data=pew,family=binomial)
summary(socialnetmodel)

snmod2<-glm(smallgov~white+rep+white*rep+INCOME+PPEDUCAT+ppagecat+SEX,data=pew,family=binomial)
summary(snmod2)

sndata = data.frame(white = rep(0:1,2), rep = rep(1:0,each =2), INCOME=mean(pew$INCOME,na.rm=TRUE), PPEDUCAT=mean(pew$PPEDUCAT, na.rm=TRUE),ppagecat=mean(pew$ppagecat,na.rm=TRUE),SEX=mean(pew$SEX,na.rm = TRUE))
save<-predict(snmod2, sndata, type="response", se.fit = TRUE)
preds<-data.frame(type = c("NW Rep", "W Rep", "NW Dem", "W Dem"),predicted = save$fit, UL = save$fit + save$se.fit, LL = save$fit - save$se.fit)

windowsFonts("Roman"=windowsFont("CM Roman"))

snp<-ggplot(preds, aes(type,predicted)) + 
  geom_point() +
  geom_pointrange(aes(ymin=LL,ymax=UL))+ 
  theme_bw()+ 
  labs(x="Identity",y="Predicted Probabilities") + 
  theme(text = element_text(size=24, family="CM Roman")) +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0))) +
  theme(axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0))) +
  scale_x_discrete(limits=c("NW Dem","W Dem","W Rep", "NW Rep"))

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

white<-rep(0:1,2)
rep<-rep(0:1,each=2)
pewnew<-data.frame(white=white,
                   rep=rep,INCOME=mean(pew$INCOME,na.rm=TRUE),PPEDUCAT=mean(pew$PPEDUCAT,na.rm=TRUE),ppagecat=mean(pew$ppagecat,na.rm=TRUE),SEX=mean(pew$SEX,na.rm=TRUE))
pewnew1<- cbind(pewnew,predict(dismodel1, pewnew, type = "probs"))
pewnewdat <- melt(pewnew1, id.vars = c("white", "rep","INCOME","PPEDUCAT","ppagecat","SEX"),
                  variable.name = "Identity", value.name="Probability")
pewnewdat$identity<-ifelse(pewnewdat$white == 1 & pewnewdat$rep == 1, "W Rep",ifelse(pewnewdat$white == 1 & pewnewdat$rep == 0 , "W Dem",ifelse(pewnewdat$white == 0 & pewnewdat$rep == 1, "NW Rep",ifelse(pewnewdat$white == 0 & pewnewdat$rep == 0, "NW Dem", NA))))

p1<-ggplot(pewnewdat, aes(x = identity, y = value)) +
  geom_point(aes(colour = factor(variable),size=2)) + theme_bw()+ labs(title="Discrimination by Identity",x="Identity",y="Predicted Probabilities") + theme(plot.title = element_text(hjust = 0.5))+ theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold")) + theme(plot.title = element_text(size = 15, face = "bold")) + scale_colour_discrete(name  ="Support", breaks=c("1", "2", "3", "4"),labels=c("Lot", "Some", "Little", "None"))

lotsdata<-subset(pewnewdat,select = c("variable","value","identity"),subset = (variable == 1 | variable == 2))
plots<-ggplot(lotsdata, aes(x = identity, y = value)) +
  geom_point(aes(shape = factor(variable)),size= 4) + 
  theme_bw()+ 
  labs(title="African Americans",x="Identity",y="Predicted Probabilities") + 
  theme(plot.title = element_text(hjust = 0.5))+ 
  theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold")) + 
  theme(plot.title = element_text(size = 15, face = "bold")) + 
  scale_shape_discrete(name  ="Discrimination", breaks=c("1", "2", "3", "4"),labels=c("Lot", "Some", "Little", "None"))+ 
  theme(legend.position="none") +
  theme(text = element_text(size=24, family="CM Roman")) +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0))) +
  theme(axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0))) +


exp(coef(dismodel1))
(ci<-confint(dismodel1))
exp(cbind(OR = coef(dismodel1), ci))

hisnew1<- cbind(pewnew,predict(dishispmodel1, pewnew, type = "probs"))
hisnewdat <- melt(hisnew1, id.vars = c("white", "rep","INCOME","PPEDUCAT","ppagecat","SEX"),
                  variable.name = "Identity", value.name="Probability")
hisnewdat$identity<-ifelse(hisnewdat$white == 1 & hisnewdat$rep == 1, "W Rep",ifelse(hisnewdat$white == 1 & hisnewdat$rep == 0 , "W Dem",ifelse(hisnewdat$white == 0 & hisnewdat$rep == 1, "NW Rep",ifelse(hisnewdat$white == 0 & hisnewdat$rep == 0, "NW Dem", NA))))

hisp1<-ggplot(hisnewdat, aes(x = identity, y = value)) +
  geom_point(aes(colour = factor(variable),size=2)) + theme_bw()+ labs(title="Discrimination by Identity",x="Identity",y="Predicted Probabilities") + theme(plot.title = element_text(hjust = 0.5))+ theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold")) + theme(plot.title = element_text(size = 15, face = "bold")) + scale_colour_discrete(name  ="Support", breaks=c("1", "2", "3", "4"),labels=c("Lot", "Some", "Little", "None"))

hislotsdata<-subset(hisnewdat,select = c("variable","value","identity"),subset = (variable == 1 | variable == 2))
hisplots<-ggplot(hislotsdata, aes(x = identity, y = value)) +
  geom_point(aes(shape = factor(variable)),size=4) + theme_bw()+ labs(title="Hispanic Americans",x="Identity",y="Predicted Probabilities") + theme(plot.title = element_text(hjust = 0.5))+ theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold")) + theme(plot.title = element_text(size = 15, face = "bold")) + scale_shape_discrete(name  ="Discrimination", breaks=c("1", "2", "3", "4"),labels=c("Lot", "Some", "Little", "None")) + theme(legend.position=c(.8,.65))+ theme(legend.background = element_rect(fill="white",size=0.5, linetype="solid",colour ="black"))

grid.arrange(plots,hisplots,ncol=2)

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

youthpolsmod<-polr(as.factor(youthpol)~white+rep+white*rep+INCOME+PPEDUCAT+ppagecat+SEX,data=pew,Hess=TRUE,model=TRUE)
summary(youthpolsmod)
coeftest(youthpolsmod)

transpolsmod<-polr(as.factor(transpol)~white+rep+white*rep+INCOME+PPEDUCAT+ppagecat+SEX,data=pew,Hess=TRUE,model=TRUE)
summary(transpolsmod)
coeftest(transpolsmod)

mar<- cbind(pewnew,predict(mar2, pewnew, type = "probs"))
mardat <- melt(mar, id.vars = c("white", "rep","INCOME","PPEDUCAT","ppagecat","SEX"),
                  variable.name = "Identity", value.name="Probability")
mardat$identity<-ifelse(mardat$white == 1 & mardat$rep == 1, "W Rep",ifelse(mardat$white == 1 & mardat$rep == 0 , "W Dem",ifelse(mardat$white == 0 & mardat$rep == 1, "NW Rep",ifelse(mardat$white == 0 & mardat$rep == 0, "NW Dem", NA))))
submar<-subset(mardat,select = c("variable","value","identity"),subset = (variable == 4))
submar$variable<-"Marriage"

hiv<-cbind(pewnew,predict(hivpolsmod,pewnew,type="probs"))
hivdat<- melt(hiv,id.vars = c("white", "rep","INCOME","PPEDUCAT","ppagecat","SEX"),
              variable.name = "Identity", value.name="Probability")
hivdat$identity<-ifelse(hivdat$white == 1 & hivdat$rep == 1, "W Rep",ifelse(hivdat$white == 1 & hivdat$rep == 0 , "W Dem",ifelse(hivdat$white == 0 & hivdat$rep == 1, "NW Rep",ifelse(hivdat$white == 0 & hivdat$rep == 0, "NW Dem", NA))))
subhiv<-subset(hivdat,select=c("variable","value","identity"),subset = (variable == 4))
subhiv$variable<-"HIV"
subtotal<-rbind(submar,subhiv)

polplot<-ggplot(subtotal, aes(x = variable, y = value,group=identity)) +
  geom_line(aes(linetype = factor(identity)),size=1) + 
  theme_bw()+
  theme(text = element_text(size=24, family="CM Roman")) +
  labs(y="Predicted Probabilities") + 
  theme(axis.text.x=element_text(size=14),axis.text.y=element_text(size=12),axis.title=element_text(size=15)) + 
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0))) +
  theme(axis.title.x = element_blank()) +
  theme(legend.position="none") + 
  scale_x_discrete(expand=c(0.14,0.14)) +
  geom_dl(aes(label = identity),method=list("last.qp", dl.trans(x=x+.2) ,cex = 0.8))

  
  polplot

#immigrant model
immmod1<-glm(immi~white+rep+white*rep+INCOME+PPEDUCAT+ppagecat+SEX,data=pew,family=binomial)
summary(immmod1)

immdata = data.frame(white = rep(0:1,2), rep = rep(1:0,each =2), INCOME=mean(pew$INCOME,na.rm=TRUE), PPEDUCAT=mean(pew$PPEDUCAT, na.rm=TRUE),ppagecat=mean(pew$ppagecat,na.rm=TRUE),SEX=mean(pew$SEX,na.rm = TRUE))
save<-predict(immmod1, immdata, type="response", se.fit = TRUE)
preds<-data.frame(type = c("NW Rep", "W Rep", "NW Dem", "W Dem"),predicted = save$fit, UL = save$fit + save$se.fit, LL = save$fit - save$se.fit)

imp<-ggplot(preds, aes(type,predicted)) + 
  geom_point() +
  geom_pointrange(aes(ymin=LL,ymax=UL))+ 
  theme_bw()+ 
  labs(x="Identity",y="Predicted Probabilities") + 
  theme(text = element_text(size=24, family="CM Roman")) +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0))) +
  theme(axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0))) +
  scale_x_discrete(limits=c("NW Dem","W Dem","W Rep", "NW Rep"))
