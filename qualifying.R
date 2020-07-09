#small govt
pew$Q4

#poor people
pew$Q9C

library(car)

pew$Q4<-recode(pew$Q4,"-1=NA")
pew$Q4<-pew$Q4-1
table(pew$Q4)
pew$Q9C<-recode(pew$Q9C,"-1=NA")
pew$Q9C<-pew$Q9C-1
table(pew$Q9C)

#race
pew$PPETHM
pew$PPETHM<-recode(pew$PPETHM,"-2=NA")
pew$PPETHM<-recode(pew$PPETHM,"-1=NA")
pew$PPETHM<-recode(pew$PPETHM,"2=0")
pew$PPETHM<-recode(pew$PPETHM,"3=0")
pew$PPETHM<-recode(pew$PPETHM,"4=0")
pew$PPETHM<-recode(pew$PPETHM,"5=0")

#Republican
pew$PARTY
pew$Rep<-pew$PARTY
pew$Rep<-recode(pew$Rep,"-1=NA")
pew$Rep<-recode(pew$Rep,"2=0")
pew$Rep<-recode(pew$Rep,"3=0")
pew$Rep<-recode(pew$Rep,"4=0")

#democrat
pew$Dem<-pew$PARTY
pew$Dem<-recode(pew$Dem,"-1=NA")
pew$Dem<-recode(pew$Dem,"1=0")
pew$Dem<-recode(pew$Dem,"2=1")
pew$Dem<-recode(pew$Dem,"3=0")
pew$Dem<-recode(pew$Dem,"4=0")

glm.fit <- glm(Q4 ~ PPETHM + Rep + Dem, data = pew, family = binomial)
summary(glm.fit)

glm2<-glm(Q9C ~ PPETHM + Rep + Dem, data = pew, family = binomial)
summary(glm2)

library(stargazer)
stargazer(glm.fit)
