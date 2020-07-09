#race
anes$white<-anes$V161310x
anes$white<-recode(anes$white,"-9=NA")
anes$white<-recode(anes$white,"2=0")
anes$white<-recode(anes$white,"3=0")
anes$white<-recode(anes$white,"4=0")
anes$white<-recode(anes$white,"5=0")
anes$white<-recode(anes$white,"6=0")

#social net policies
anes$jobincome<-anes$V161189
anes$jobincome<-recode(anes$jobincome,"-9=NA")
anes$jobincome<-recode(anes$jobincome,"-8=NA")
anes$jobincome<-recode(anes$jobincome,"99=NA")
anes$welfare<-anes$V161209
anes$welfare<-recode(anes$welfare,"-9=NA")
anes$welfare<-recode(anes$welfare,"-8=NA")
anes$welfare<-recode(anes$welfare,"3=4")
anes$welfare<-recode(anes$welfare,"2=3")
anes$welfare<-recode(anes$welfare,"4=2")
anes$incomegap<-anes$V162148
anes$incomegap<-recode(anes$incomegap,"-9=NA")
anes$incomegap<-recode(anes$incomegap,"-8=NA")
anes$incomegap<-recode(anes$incomegap,"-7=NA")
anes$incomegap<-recode(anes$incomegap,"-6=NA")
anes$incomegap<-recode(anes$incomegap,"3=4")
anes$incomegap<-recode(anes$incomegap,"2=3")
anes$incomegap<-recode(anes$incomegap,"4=2")

#discrimination
anes$bdis<-anes$V162357
anes$bdis<-recode(anes$bdis,"-9=NA")
anes$bdis<-recode(anes$bdis,"-7=NA")
anes$bdis<-recode(anes$bdis,"-6=NA")
anes$bdis<-recode(anes$bdis,"-5=NA")
anes$hdis<-anes$V162358
anes$hdis<-recode(anes$hdis,"-9=NA")
anes$hdis<-recode(anes$hdis,"-7=NA")
anes$hdis<-recode(anes$hdis,"-6=NA")
anes$hdis<-recode(anes$hdis,"-5=NA")
anes$adis<-anes$V162359
anes$adis<-recode(anes$adis,"-9=NA")
anes$adis<-recode(anes$adis,"-7=NA")
anes$adis<-recode(anes$adis,"-6=NA")
anes$adis<-recode(anes$adis,"-5=NA")
anes$wdis<-anes$V162360
anes$wdis<-recode(anes$wdis,"-9=NA")
anes$wdis<-recode(anes$wdis,"-7=NA")
anes$wdis<-recode(anes$wdis,"-6=NA")
anes$wdis<-recode(anes$wdis,"-5=NA")
anes$tdis<-anes$V162366
anes$tdis<-recode(anes$tdis,"-9=NA")
anes$tdis<-recode(anes$tdis,"-7=NA")
anes$tdis<-recode(anes$tdis,"-6=NA")
anes$tdis<-recode(anes$tdis,"-5=NA")

#stereotypes
anes$bwork<-anes$V162346
anes$bwork<-recode(anes$bwork,"-9=NA")
anes$bwork<-recode(anes$bwork,"-7=NA")
anes$bwork<-recode(anes$bwork,"-6=NA")
anes$bwork<-recode(anes$bwork,"-5=NA")
anes$hwork<-anes$V162347
anes$hwork<-recode(anes$hwork,"-9=NA")
anes$hwork<-recode(anes$hwork,"-7=NA")
anes$hwork<-recode(anes$hwork,"-6=NA")
anes$hwork<-recode(anes$hwork,"-5=NA")
anes$awork<-anes$V162348
anes$awork<-recode(anes$awork,"-9=NA")
anes$awork<-recode(anes$awork,"-7=NA")
anes$awork<-recode(anes$awork,"-6=NA")
anes$awork<-recode(anes$awork,"-5=NA")
anes$wwork<-anes$V162345
anes$wwork<-recode(anes$wwork,"-9=NA")
anes$wwork<-recode(anes$wwork,"-7=NA")
anes$wwork<-recode(anes$wwork,"-6=NA")
anes$wwork<-recode(anes$wwork,"-5=NA")
anes$wviolent<-anes$V162349
anes$wviolent<-recode(anes$wviolent,"-9=NA")
anes$wviolent<-recode(anes$wviolent,"-7=NA")
anes$wviolent<-recode(anes$wviolent,"-6=NA")
anes$wviolent<-recode(anes$wviolent,"-5=NA")
anes$bviolent<-anes$V162350
anes$bviolent<-recode(anes$bviolent,"-9=NA")
anes$bviolent<-recode(anes$bviolent,"-7=NA")
anes$bviolent<-recode(anes$bviolent,"-6=NA")
anes$bviolent<-recode(anes$bviolent,"-5=NA")
anes$hviolent<-anes$V162351
anes$hviolent<-recode(anes$hviolent,"-9=NA")
anes$hviolent<-recode(anes$hviolent,"-7=NA")
anes$hviolent<-recode(anes$hviolent,"-6=NA")
anes$hviolent<-recode(anes$hviolent,"-5=NA")
anes$aviolent<-anes$V162352
anes$aviolent<-recode(anes$aviolent,"-9=NA")
anes$aviolent<-recode(anes$aviolent,"-7=NA")
anes$aviolent<-recode(anes$aviolent,"-6=NA")
anes$aviolent<-recode(anes$aviolent,"-5=NA")

#party
anes$party<-anes$V161158x
anes$party<-recode(anes$party,"-9=NA")
anes$party<-recode(anes$party,"-8=NA")
anes$party<-recode(anes$party,"-2=NA")

#controls
anes$income<-anes$V161361x
anes$income<-recode(anes$income,"-9=NA")
anes$income<-recode(anes$income,"-5=NA")
anes$edu<-anes$V161270
anes$edu<-recode(anes$edu,"-9=NA")
anes$edu<-recode(anes$edu,"-8=NA")
anes$edu<-recode(anes$edu,"90=NA")
anes$edu<-recode(anes$edu,"95=NA")
anes$age<-anes$V161267
anes$age<-recode(anes$age,"-9=NA")
anes$age<-recode(anes$age,"-8=NA")

#gender
anes$male<-anes$V161342
anes$male<-recode(anes$male,"-9=NA")
anes$male<-recode(anes$male,"-8=NA")
anes$male<-recode(anes$male,"2=0")
anes$male<-recode(anes$male,"3=0")

#gay
anes$lgb<-anes$V161511
anes$lgb<-recode(anes$lgb,"-9=NA")
anes$lgb<-recode(anes$lgb,"-5=NA")
anes$lgb<-recode(anes$lgb,"1=0")
anes$lgb<-recode(anes$lgb,"2=1")
anes$lgb<-recode(anes$lgb,"3=1")


table(anes$white,anes$lgb,anes$income)

#social net models
jobincomemod<-lm(jobincome~white+lgb+white*lgb+party+income+edu+age+male,data=anes)
summary(jobincomemod)

welfaremod<-polr(as.factor(welfare)~white+lgb+white*lgb+party+income+edu+age+male,data=anes,Hess=TRUE,model=TRUE)
summary(welfaremod)
coeftest(welfaremod)
incomegapmod<-polr(as.factor(incomegap)~white+lgb+white*lgb+party+income+edu+age+male,data=anes)
summary(incomegapmod)
coeftest(incomegapmod)

#discrimination models
bdismod<-polr(as.factor(bdis)~white+lgb+white*lgb+party+income+edu+age+male,data=anes,Hess=TRUE,model=TRUE)
summary(bdismod)
coeftest(bdismod)
hdismod<-polr(as.factor(hdis)~white+lgb+white*lgb+party+income+edu+age+male,data=anes,Hess=TRUE,model=TRUE)
summary(hdismod)
coeftest(hdismod)
adismod<-polr(as.factor(adis)~white+lgb+white*lgb+party+income+edu+age+male,data=anes,Hess=TRUE,model=TRUE)
summary(adismod)

#sterotype models
bworkmod<-lm(bwork~white+lgb+white*lgb+party+income+edu+age+male,data=anes)
summary(bworkmod)
hworkmod<-lm(hwork~white+lgb+white*lgb+party+income+edu+age+male,data=anes)
summary(hworkmod)
aworkmod<-lm(awork~white+lgb+white*lgb+party+income+edu+age+male,data=anes)
summary(aworkmod)
wworkmod<-lm(wwork~white+lgb+white*lgb+party+income+edu+age+male,data=anes)
summary(wworkmod)
wviolentmod<-lm(wviolent~white+lgb+white*lgb+party+income+edu+age+male,data=anes)
summary(wviolentmod)
bviolentmod<-lm(bviolent~white+lgb+white*lgb+party+income+edu+age+male,data=anes)
summary(bviolentmod)
hviolentmod<-lm(hviolent~white+lgb+white*lgb+party+income+edu+age+male,data=anes)
summary(hviolentmod)
aviolentmod<-lm(aviolent~white+lgb+white*lgb+party+income+edu+age+male,data=anes)
summary(aviolentmod)

#point estimates
wsjob<-0.362423
nwgjob<-(-0.325113)
wgjob<-0.362423-0.325113-.453184
wswel<-.329622
nwgwel<-(-.387403)
wgwel<-.362423-.387403-.499822
wsinc<-(-0.074977)
nwginc<-(-.215057)
wginc<-(-.074977)-.215057-.173624
wsbdis<-0.631500
nwgbdis<-(-0.537390)
wgbdis<-.6315-0.53739-.328778
wshdis<-.304439
nwghdis<-(-.698128)
wghdis<-.304439-.698128+.040136
wsbwork<-.1707196
nwgbwork<-(-.4892224)
wgbwork<-.1707196-.4892224+.2963802
wshwork<-.350271
nwghwork<-(-.111251)
wghwork<-.350271-.111251+.110849
wsbvio<-.001948
nwgbvio<-(-.341897)
wgbvio<-.001948-.341897+.289720
wshvio<-.001948
nwghvio<-(-.341897)
wghvio<-.001948-.341897+.289720

#data visualization
p1<-ggplot(data=Point, aes(x=id)) +
  geom_errorbar(aes(ymax=UL,ymin=LL))
p2<-ggplot(data=Point2, aes(x=id)) +
  geom_errorbar(aes(ymax=UL,ymin=LL))
p3<-ggplot(data=Point3, aes(x=id)) +
  geom_errorbar(aes(ymax=UL,ymin=LL))
p4<-ggplot(data=Point4, aes(x=id)) +
  geom_errorbar(aes(ymax=UL,ymin=LL))
p5<- ggplot(data=Point5, aes(x=id)) +
  geom_errorbar(aes(ymax=UL,ymin=LL))
p6<- ggplot(data=Point6, aes(x=id)) +
  geom_errorbar(aes(ymax=UL,ymin=LL))
p7<- ggplot(data=Point7, aes(x=id)) +
  geom_errorbar(aes(ymax=UL,ymin=LL))
p8<- ggplot(data=Point8, aes(x=id)) +
  geom_errorbar(aes(ymax=UL,ymin=LL))
p9<- ggplot(data=Point9, aes(x=id)) +
  geom_errorbar(aes(ymax=UL,ymin=LL))
                