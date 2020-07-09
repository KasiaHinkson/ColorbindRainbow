rm(list=ls())
lgbt<-pew2013lgbtpublicdatarelease_2_
attach(lgbt)

#Opinion Obama (About equal very/mostly favor but not universal)
hist(Q3_A)
#Opinion Dem Party (mostly favor)
hist(Q3_B)
#Opinion Rep Party (almost all very/mostly unfavor)
hist(Q3_C)
#Opinion Supreme Court (mostly favor)
hist(Q3_D)

#Govt size (483 smaller, 702 bigger)
table(Q4)

#follow govt (yes)
table(Q5)

#discrimination
hist(Q6_A)
hist(Q6_B)
hist(Q6_C)
hist(Q6_D)
hist(Q6_E)

#gun ownership (about 2 to 1)
hist(Q8)

#immigration (about 2 to 1)
hist(Q9A)

#trust (about even)
hist(Q9B)

#poor people (about 2 to 1 poor people have it hard)
hist(Q9C)

#gay marriage (basically unified support)
hist(Q11)
table(Q11,Q24)
table(Q11,Q25)

#gay adoption (same)
hist(Q12)

#vote (they say they do)
hist(OFTVOTE)

#social network (over 200 say no)
hist(SNS)

#partyid
hist(PARTY)
table<-table(PARTY)
prop.table(table)
table2<-table(PARTYLN)
prop.table(table2)

#ideology
table3<-table(IDEO)
prop.table(table3)
hist(IDEO)

#sexual orientation
hist(Q24)
table4<-table(Q25,Q24)
prop.table(table4)

#transgender (only 45 trans, and only 17 chose to be referred to as trans instead of orientation)
table(Q25)
table(QR2)

#summary
table(LGBT)

#id importance (it's textbook normal)
hist(Q31)
table5<-table(Q31,PARTY)
prop.table(table5)

#id is good or bad (most say no diff, very few say bad)
hist(Q32)
table6<-table(Q32,PARTY)
prop.table(table6,2)
prop.table(table6,1)

#come out to mom
table(Q44)
#come out to dad
table(Q45)
#both (looks like most did same for both parents)
table7<-table(Q44,Q45)
prop.table(table7)
#come out to sisters
table(Q46)
#come out to brothers
table(Q47)

#measure of just being out in general
table8<-table(Q49)
prop.table(table8)

#most important issues
table9<-table(q52oe_1)
table10<-table(q52oe_2)
round(prop.table(table9)*100,2)
round(prop.table(table10)*100,2)

#existing social acceptance
table(Q53)

#compared to 10 years ago
table(Q54)

#10 years from now
table(Q55)

#how does groupx feel towards lgbt?
#dem party (2 to 1 friendly v neutral, 80 unfriendly)
table(Q56_A)
#rep party (far more agreement)
table(Q56_B)
#obama (basically same as dem, slightly more positive)
table(Q56_C)

#what helps gain acceptance
table(Q63_A) #knowing lgbt yes
table(Q63_B) #lgbt tv characters more mixed
table(Q63_C) #open lgbt public figures yeah
table(Q63_D) #lgbt familieis more mixed
table(Q63_E) #open support from non lgbt public figures yes
table(Q63_F) #pride events, not really
accept<-cbind(Q63_A,Q63_B,Q63_C,Q63_D,Q63_E,Q63_F)

#social acceptance of subgroups
#gay men - mode is "some" followed by "only a little"
table(Q65_A)
#lesbians - about the same, a little bit more acceptance perceived
table(Q65_B)
#bi men - less acceptance
table(Q65_C)
#bi women - apparently really accepted
table(Q65_D)
#trans - nope
table(Q65_E)

#oh baby, religion feelings towards lgbt
#evangelical - lol unfriendly, no shit (211 say neutral tho)
table(Q66_A)
#Catholic -more unfriendly than evangelical but in 2013
table(Q66_B)
#Jewish - neutral
table(Q66_C)
#Muslim - super unfriendly
table(Q66_D)
#Mormon - super unfriendly
table(Q66_E)
#non evangelical protestants - neutral
table(Q66_F)

#conflict with religious id
table(Q67)
table(Q68)

#religious id
table14<-table(religNEW)
round(prop.table(table14)*100,2)
#christian if something else
table(CHR)
#born again - 162 yes...498 no
table(BORN)

#religious attendance - low
table(ATTEND)
#importance of religion - mixed, but mostly not important
table(IMP)


#social acceptance where you live (normal-esque)
table(Q71)
hist(Q71)

#lgbt neighborhood
table(Q72)
table11<-table(Q72,PARTY)
prop.table(table10,2)

#lgbt friends - this one could be really good to look at; we know contact changes non-lgbt opinion so why wouldn't it change lgbt opinion too?
table(Q74)
table12<-table(Q74,PARTY)
prop.table(table12,1)

#policy issues - this is definitely important, compare levels of variance
#equal employment - top
table(Q80_A)
#marriage - top
table(Q80_B)
#adoption - v important
table(Q80_C)
#AIDS - v important
table(Q80_D)
#civil unions - important
table(Q80_E)
#lgbt youth services - v important
table(Q80_F)
# trans health issues - super mixed
table(Q80_G)

#q82 is acts of discrimination

#lgbt specific political participation - i need to turn this into one variable
#member of lgbt organization - most haven't
table(Q83_A)
#bought product bc company supported lgbt - about even across
table(Q83_B)
#boycotted bc didn't support - mixed
table(Q83_C)
#rally/march - nope, and def not in last year
table(Q83_D)
#pride - more mixed
table(Q83_E)
#donate money to politics - nope
table(Q83_F)

#talk lgbt issues online - nope
table(Q85)

#out online - evenly split, slightly more yes. also doesn't look like republicans are extra out online, looks like the opposite if anything
table(Q85a)
table13<-table(Q85a,PARTY)
prop.table(table13,2)

#accepting workplace
table(Q98)

#out at work - VERY mixed
table(Q99)

#gay marriage has taken too much focus/is top priority (about 2 to 1)
table(Q102A)
#mainstream v distinct - very evenly split
table(Q102B)
#different or not from others - 3 to 1 don't want to be seen as diff
table(Q102C)
#importance of gay spaces - split
table(Q102D)

#registered to vote - they say yes
table(REG)

#income - spikes at under 20,000 and at 40,000
table15<-table(INCOME)
round(prop.table(table15)*100,2)
hist(INCOME)

#education - highly educated
hist(PPEDUCAT)

#race - they're all white
hist(PPETHM)