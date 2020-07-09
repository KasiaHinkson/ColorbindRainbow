######## Multinomial Logit Model #######

par(mfrow=c(1,2))
hist(PARTY,main = "Party Identification",xlab="Party",breaks = rep(1:3,each=2)+c(-.4,.4),cex.lab=1.5,xaxt="n")
box()
axis(1,at=seq(1,3,1),labels=c("Republican","Democrat","Independent"))

hist(IDEO,main="Ideology",xlab="Ideology",breaks=rep(1:5,each=2)+c(-.5,.4),cex.lab=1.5,xaxt="n")
axis(1,at=seq(1,5,1),labels=c("V. Conservative","","Moderate","","V. Liberal"))
box()
attach(lgbtreal)


agemodel<-multinom(PARTY~Q31+lgbt.out+lgbt.policy+Q67+ATTEND+lgbt.part+INCOME+PPEDUCAT+SEX+ppagecat,lgbtreal)
summary(agemodel)
agemodel2<-vglm(PARTY~Q31+lgbt.out+lgbt.policy+Q67+ATTEND+lgbt.part+INCOME+PPEDUCAT+SEX+ppagecat,multinomial,data=lgbtreal)
summary(agemodel2)

agemod1<-multinom(PARTY~Q31+lgbt.out+IDEO+Q67+ATTEND+lgbt.part+INCOME+PPEDUCAT+SEX+ppagecat,lgbtreal)
summary(agemod1)
agemod2<-vglm(PARTY~Q31+lgbt.out+IDEO+Q67+ATTEND+lgbt.part+INCOME+PPEDUCAT+SEX+ppagecat,multinomial,data=lgbtreal)
summary(agemod2)


idmodel<-multinom(PARTY~Q31+lgbt.out+lgbt.policy+Q67+ATTEND+INCOME+PPEDUCAT+SEX+ppagecat,lgbtreal)
summary(idmodel)
idmod1<-vglm(PARTY~Q31+lgbt.out+lgbt.policy+Q67+ATTEND+INCOME+PPEDUCAT+SEX+ppagecat,multinomial,data=lgbtreal)
summary(idmod1)

partmodel<-multinom(PARTY~lgbt.part+lgbt.out+lgbt.policy+Q67+ATTEND+INCOME+PPEDUCAT+SEX+ppagecat,lgbtreal)
summary(partmodel)
partmod1<-vglm(PARTY~lgbt.part+lgbt.out+lgbt.policy+Q67+ATTEND+INCOME+PPEDUCAT+SEX+ppagecat,multinomial,data=lgbtreal)
summary(partmod1)

lgbtnew<-data.frame(ATTEND=rep(rep(1:6,each=1)),
                    Q31=mean(Q31),lgbt.policy=mean(lgbt.policy),lgbt.out=mean(lgbt.out),INCOME=mean(INCOME))
lgbtnew1<- cbind(lgbtnew,predict(model1a, lgbtnew, type = "probs",se.fit=TRUE))
lgbtnewdat <- melt(lgbtnew1, id.vars = c("Q31", "ATTEND","lgbt.policy","lgbt.out","INCOME"),
                variable.name = "Party", value.name="Probability")
p<- ggplot(lgbtnewdat, aes(x = ATTEND, y = value, group = variable)) +
  geom_line(aes(linetype=factor(variable,labels=c("Republican","Democrat","Independent")))) + theme_bw() + labs(title="Party ID by Religious Attendance",x="Religious Attendance",y="Predicted Probabilities",linetype="Party ID") + theme(plot.title = element_text(hjust = 0.5))+ theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold")) + theme(plot.title = element_text(size = 15, face = "bold"))  + theme(legend.position = "bottom") + theme(
    legend.box.background = element_rect(colour = "black",  linetype = "solid"))    


lgbtnewa<-data.frame(ATTEND=mean(ATTEND),
                    Q31=seq(1,5,1),lgbt.policy=mean(lgbt.policy),lgbt.out=mean(lgbt.out),INCOME=mean(INCOME))
lgbtnew1a<- cbind(lgbtnewa,predict(model1a, lgbtnewa, type = "probs",se.fit=TRUE))
lgbtnewdata <- melt(lgbtnew1a, id.vars = c("Q31", "ATTEND","lgbt.policy","lgbt.out","INCOME"),
                   variable.name = "Party", value.name="Probability")
pa<- ggplot(lgbtnewdata, aes(x = Q31, y = value, group = variable)) +
  geom_line(aes(linetype=factor(variable,labels=c("Republican","Democrat","Independent")))) + theme_bw() + labs(title="Party ID by LGBT ID Importance",x="LGBT ID Importance",y="Predicted Probabilities",linetype="Party ID") + theme(plot.title = element_text(hjust = 0.5))+ theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold")) + theme(plot.title = element_text(size = 15, face = "bold"))  + theme(legend.position = c(0.8, 0.75)) + theme(
    legend.box.background = element_rect(colour = "black",  linetype = "solid")) 

outnew<-data.frame(ATTEND=mean(ATTEND),
                     Q31=mean(Q31),lgbt.policy=mean(lgbt.policy),lgbt.out=seq(-2,1,.1),INCOME=mean(INCOME))
outnew1<- cbind(outnew,predict(model1a, outnew, type = "probs",se.fit=TRUE))
outnewdata <- melt(outnew1, id.vars = c("Q31", "ATTEND","lgbt.policy","lgbt.out","INCOME"),
                    variable.name = "Party", value.name="Probability")
out<- ggplot(outnewdata, aes(x = lgbt.out, y = value, group = variable)) +
  geom_line(aes(linetype=factor(variable,labels=c("Republican","Democrat","Independent")))) + theme_bw() + labs(title="Party ID by Out Factor",x="Out Factor Score",y="Predicted Probabilities",linetype="Party ID") + theme(plot.title = element_text(hjust = 0.5))+ theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold")) + theme(plot.title = element_text(size = 15, face = "bold"))  + theme(legend.position = c(0.8, 0.75)) + theme(
    legend.box.background = element_rect(colour = "black",  linetype = "solid")) 
