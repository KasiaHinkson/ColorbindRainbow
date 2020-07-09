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

