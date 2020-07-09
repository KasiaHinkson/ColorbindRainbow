xtabs( ~ predict(model1) + PARTY)

lgbtnot<-data.frame(PARTY,IDEO,lgbt$idimport,lgbt$policyfactora,lgbt$q67a,lgbt$attend1,lgbt$isolationfactora,lgbt$partfactora,lgbt$outfactora,INCOME,PPEDUCAT,SEX)
lgbtnot<-na.omit(lgbtnot)

lgbtmodel1<-multinom(PARTY~lgbt.idimport+lgbt.policyfactora+lgbt.q67a+lgbt.attend1+lgbt.isolationfactora+lgbt.partfactora+lgbt.outfactora+INCOME+PPEDUCAT+SEX,lgbtnot)
summary(lgbtmodel1)

xtabs(~predict(lgbtmodel1) + lgbtnot$PARTY)
(7+315+69)/nrow(lgbtnot)
