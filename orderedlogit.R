####### Ordered Logit ########


model3<-polr(as.factor(IDEO)~Q31+lgbt.out+Q67+ATTEND+lgbt.part+INCOME+PPEDUCAT+SEX+ppagecat,data=lgbtreal,Hess=TRUE,model=TRUE)
summary(model3)
model3a<-step(model3)



idmod3<-polr(as.factor(IDEO)~Q31+lgbt.out+Q67+ATTEND+INCOME+PPEDUCAT+SEX+ppagecat,data=lgbtreal,Hess=TRUE,model=TRUE)
summary(idmod3)
partmod3<-polr(as.factor(IDEO)~lgbt.part+lgbt.out+Q67+ATTEND+INCOME+PPEDUCAT+SEX+ppagecat,data=lgbtreal,Hess=TRUE,model=TRUE)
summary(partmod3)


exp(coef(model3))
(ci<-confint(model3))
exp(cbind(OR = coef(model3), ci))

participate<-seq(-2,2,length=100)
lognew<-data.frame(lgbt.part=participate,
                    Q67=0,ATTEND=mean(ATTEND),ppagecat=mean(ppagecat),PPEDUCAT=mean(PPEDUCAT))
lognew1<- cbind(lognew,predict(model3a, lognew, type = "probs"))
lognewdat <- melt(lognew1, id.vars = c("lgbt.part", "Q67","ATTEND","ppagecat","PPEDUCAT"),
                   variable.name = "Party", value.name="Probability")

p1<-ggplot(lognewdat, aes(x = lgbt.part, y = value, linetype = variable)) +
  geom_line(aes(linetype=factor(variable,labels=c("V. Con","Conservative","Moderate","Liberal","V. Lib")))) + theme_bw()+ labs(title="Ideology by Participation - No Conflict",x="LGBT Participation",y="Predicted Probabilities",linetype="Ideology") + theme(plot.title = element_text(hjust = 0.5))+ theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold")) + theme(plot.title = element_text(size = 15, face = "bold"))  + theme(legend.position = "none") +theme(legend.box.background = element_rect(colour = "black",  linetype = "solid"))

outnew<-data.frame(lgbt.part=participate,
                     Q67=1,ATTEND=mean(ATTEND),ppagecat=mean(ppagecat),PPEDUCAT=mean(PPEDUCAT))
outnew1<- cbind(outnew,predict(model3a, outnew, type = "probs"))
outnewdat <- melt(outnew1, id.vars = c("lgbt.part", "Q67","ATTEND","ppagecat","PPEDUCAT"),
                   variable.name = "Party", value.name="Probability")

p1a<-ggplot(outnewdat, aes(x = lgbt.part, y = value, linetype = variable)) +
  geom_line(aes(linetype=factor(variable,labels=c("Very Conservative","Conservative","Moderate","Liberal","Very Liberal")))) + theme_bw()+ labs(title="Ideology by Participation - Conflict",x="LGBT Participation",y="",linetype="Ideology") + theme(plot.title = element_text(hjust = 0.5))+ theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold")) + theme(plot.title = element_text(size = 15, face = "bold"))  + theme(legend.position = "bottom") + theme(
    legend.box.background = element_rect(colour = "black",  linetype = "solid"))

prow <- plot_grid( p1 + theme(legend.position="none"),
                   p1a + theme(legend.position="none"),
                   align = 'vh',
                   hjust = -1,
                   nrow = 1
)
legend <- get_legend(p1 + theme(legend.position="bottom"))
plot <- plot_grid( prow, legend, ncol=1, rel_heights = c(2, .2))
plot





attendance<-rep(1:6,1)
orderednew<-data.frame(lgbt.part=mean(lgbt.part),
                   Q67=1,ATTEND=attendance,ppagecat=mean(ppagecat),PPEDUCAT=mean(PPEDUCAT))
orderednew1<-cbind(orderednew,predict(model3a,orderednew,type="probs"))
orderednewdat<-melt(orderednew1,id.vars=c("lgbt.part", "Q67","ATTEND","ppagecat","PPEDUCAT"), variable.name= "Party",value.name="Probability")

pb<-ggplot(orderednewdat,aes(x=ATTEND,y=value,linetype=variable)) + 
  geom_line(aes(linetype=factor(variable,labels=c("V. Con","Conservative","Moderate","Liberal","V. Lib")))) + theme_bw()+ labs(title="Ideology by Religious Attendance",x="Religious Attendance",y="",linetype="Ideology") + theme(plot.title = element_text(hjust = 0.5))+ theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold")) + theme(plot.title = element_text(size = 15, face = "bold"))  + theme(legend.position = "bottom") + theme(
    legend.box.background = element_rect(colour = "black",  linetype = "solid")) 

grid.arrange(p,pb,ncol=2)

prowb <- plot_grid( p + theme(legend.position="bottom"),
                   pb + theme(legend.position="bottom"),
                   align = 'vh',
                   hjust = -1,
                   nrow = 1
)
legendb <- get_legend(p + theme(legend.position="right"))
plotb <- plot_grid( prowb, legendb, rel_widths = c(3, .3))
plotb
