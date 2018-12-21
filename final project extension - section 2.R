load("C:/Users/Hanna/Downloads/final_data.RData")

library(rbounds)
library(lme4)
library(Matching)
library(rgenoud)

names(final_com)
X <- cbind(final_com$age,final_com$black,final_com$hispanic,final_com$two_parents,final_com$mom_edu,final_com$sibs,final_com$res_yrs,final_com$
  depression_c,final_com$emotionality,final_com$impulsivity,final_com$sociability,final_com$sub_abuse,final_com$exp_suic
,final_com$CPOV90,final_com$CFORBORN,final_com$fam_attach*final_com$collective)

Tr<- final_com$female
Y<- final_com$suicide
genout<-GenMatch(Tr=Tr, X=X,BalanceMatrix=X, estimand="ATT", M=1,pop.size = 200, max.generations=100,
                 wait.generations=4)
mout<-Match(Tr=Tr, X=X, estimand = "ATT", Weight.matrix = genout)
summary(mout)
mb<-MatchBalance(final_com$female~final_com$age+final_com$black+final_com$hispanic+final_com$two_parents+final_com$mom_edu,final_com$sibs,final_com$res_yrs,final_com$
depression_c+final_com$emotionality+final_com$impulsivity+final_com$sociability+final_com$sub_abuse+final_com$exp_suic
+final_com$CPOV90_c+final_com$CFORBORN+final_com$fam_attach*final_com$collective, match.out = mout, nboots = 500)

summary(mb)

mout_ii<- Match(Y=final_com$suicide,Tr=Tr, X=X, estimand = "ATT", Weight.matrix = genout)
summary(mout_ii)


#final_com$sub_abuse[which(final_com$sub_abuse!=0)]<-1


###PARTII-FOR SUBSTANCE ABUSE



X2 <- cbind(final_com$female,final_com$age,final_com$black,final_com$hispanic,final_com$two_parents,final_com$mom_edu,final_com$sibs,final_com$res_yrs,final_com$
             depression_c,final_com$emotionality,final_com$impulsivity,final_com$sociability,final_com$exp_suic
           ,final_com$CPOV90,final_com$CFORBORN,final_com$fam_attach*final_com$collective)

Tr2<- final_com$sub_abuse
Y<- final_com$suicide
genout2<-GenMatch(Tr=Tr2, X=X2,BalanceMatrix=X, estimand="ATT", M=1,pop.size = 200, max.generations=100,
                 wait.generations=4)
mout2<-Match(Tr=Tr2, X=X2, estimand = "ATT", Weight.matrix = genout2)
summary(mout2)
mb2<-MatchBalance(final_com$sub_abuse~final_com$female+final_com$age+final_com$black+final_com$hispanic+final_com$two_parents+final_com$mom_edu,final_com$sibs,final_com$res_yrs,final_com$
                   depression_c+final_com$emotionality+final_com$impulsivity+final_com$sociability+final_com$exp_suic
                 +final_com$CPOV90_c+final_com$CFORBORN+final_com$fam_attach*final_com$collective, match.out = mout2, nboots = 500)

summary(mb2)

mout2_ii<- Match(Y=final_com$suicide,Tr=Tr2, X=X2, estimand = "ATT", Weight.matrix = genout2)
summary(mout2_ii)



psens(mout_ii)

psens(mout2_ii)


#lines(density(non_sub_abuse_suicide), col = "blue", lwd = 2)
new_set_female<- final_com[which(final_com$female==1),]
new_set_male<- final_com[which(final_com$female==0),]

# Multilevel, All observation, Collective efficacy-family attachment interaction

model_female <- glmer(suicide ~ female_c+age_c+black_c+hispanic_c+two_parents_c+mom_edu_c+sibs_c+res_yrs_c+
                        depression_c+emotionality_c+impulsivity_c+sociability_c+sub_abuse_c+exp_suic_c
                      +CPOV90_c+CFORBORN_c+fam_attach_c*collective_c+(1+fam_attach_c|nc),
                      family = binomial("logit"), new_set_female)
sum_female <- summary(model_female)
sum_female


model_male <- glmer(suicide ~ female_c+age_c+black_c+hispanic_c+two_parents_c+mom_edu_c+sibs_c+res_yrs_c+
                      depression_c+emotionality_c+impulsivity_c+sociability_c+sub_abuse_c+exp_suic_c
                    +CPOV90_c+CFORBORN_c+fam_attach_c*collective_c+(1+fam_attach_c|nc),
                    family = binomial("logit"), new_set_male)
sum_male <- summary(model_male)
sum_male




