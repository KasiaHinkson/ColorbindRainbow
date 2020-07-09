#imm visuals

immi = data.frame(white = rep(0:1,2), rep = rep(1:0,each =2), INCOME=mean(pew$INCOME,na.rm=TRUE), PPEDUCAT=mean(pew$PPEDUCAT, na.rm=TRUE),ppagecat=mean(pew$ppagecat,na.rm=TRUE),SEX=mean(pew$SEX,na.rm = TRUE))
save<-predict(immmod1, immi, type="response", se.fit = TRUE)
preds<-data.frame(type = c("NW Rep", "White Rep", "NW Dem", "White Dem"),predicted = save$fit, UL = save$fit + save$se.fit, LL = save$fit - save$se.fit)

imp<-ggplot(preds, aes(type,predicted)) + 
  geom_point(size=1) +
  geom_pointrange(aes(ymin=LL,ymax=UL),size=1)+ 
  theme_bw()+ 
  theme(text = element_text(family="CM Roman"))+
  labs(y="Predicted Probabilities") + 
  theme(axis.text.x=element_text(size=14),axis.text.y= element_text(size=12),axis.title=element_text(size=15)) + 
  theme(axis.title.x = element_blank()) 
imp  
