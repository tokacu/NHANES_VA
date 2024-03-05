library(Hmisc)
nhanes <- sasxport.get("C:/Users/tokacu/Downloads/ACQ_G.xpt")
# character variables are converted to R

Demo<- sasxport.get("C:/Users/tokacu/OneDrive - University of Missouri/TT/Research/Publications/NHANES/DEMO_I.XPT")
Dep<- sasxport.get("C:/Users/tokacu/OneDrive - University of Missouri/TT/Research/Publications/NHANES/Depression.XPT")
Sleep<-sasxport.get("C:/Users/tokacu/OneDrive - University of Missouri/TT/Research/Publications/NHANES/SleepingDis.XPT")
Act<-sasxport.get("C:/Users/tokacu/OneDrive - University of Missouri/TT/Research/Publications/NHANES/PhysicalAct.XPT")
Chol<- sasxport.get("C:/Users/tokacu/OneDrive - University of Missouri/TT/Research/Publications/NHANES/Cholesterol.XPT")
Alch<- sasxport.get("C:/Users/tokacu/OneDrive - University of Missouri/TT/Research/Publications/NHANES/Alcoholuse.XPT")
#kidney<-sasxport.get("C:/Users/tokacu/OneDrive/Documents/TT/Research/Publications/NHANES/KidneyCond.XPT")
#Urkidney<-sasxport.get("C:/Users/tokacu/OneDrive/Documents/TT/Research/Publications/NHANES/Urine_Kidney.XPT")
Cigar<-sasxport.get("C:/Users/tokacu/OneDrive - University of Missouri/TT/Research/Publications/NHANES/SmokingCigarette.XPT")
#Tobac<-sasxport.get("C:/Users/tokacu/OneDrive/Documents/TT/Research/Publications/NHANES/SmokingTobacco.XPT")
##Merging the data
a<-Demo[Demo$seqn%in%Dep$seqn,]
s<-Sleep[Sleep$seqn%in%a$seqn,]
Tot<-merge(a,s,by="seqn")
c<-Chol[Chol$seqn%in%a$seqn,]
Tot<-merge(Tot,c,by="seqn")
k<-Alch[Alch$seqn%in%Tot$seqn,]
d<-Dep[Dep$seqn%in%Tot$seqn,]
#k<-Urkidney[Urkidney$seqn%in%kidney$seqn,]
kid<-merge(k,d,by="seqn")
Tot<-Tot[Tot$seqn%in%kid$seqn,]
Tot<-merge(Tot,kid,by="seqn")
Tot<-na.omit(Tot)
write.csv(Tot, "C:/Users/tokacu/OneDrive - University of Missouri/TT/Research/Publications/NHANes/VA_NHANES_Data.csv")

#####################################################
nhanes_s <- sasxport.get("C:/Users/tokacu/OneDrive - University of Missouri/TT/Research/NINR Workshop/2019 NINR Boot Camp/2019 NINR Boot Camp/Speaker Presentations - Thursday July 18/1_45 - Breakout - Fakhouri/SLQ_H.XPT")
nhanes$wtmec2yr
nhanes$sdmvstra
nhanes$sdmvpsu
#nhanes$linearized
a<-nhanes[nhanes$seqn%in%nhanes_s$seqn,]
b<-nhanes_s
aldat<-cbind(a,b)
tst<-aldat[,c("slq050","wtmec2yr","sdmvstra","sdmvpsu")]
library(ggplot2)
ggplot(tst, aes(x = wtmec2yr)) +
  geom_histogram(aes(color = "slq050"), fill = "white",
                 position = "identity", bins = 30) +
  scale_color_manual(values = c("#00AFBB", "#E7B800"))


###NHANES package- twitter study
library(RNHANES)
#files <- nhanes_data_files()
#variables <- nhanes_variables()
#data1516 <- nhanes_load_data("DEMO_H", "2013-2014", demographics = TRUE)
Slph<-Sleep[,c(1,4)]
a<-Demo[Demo$seqn%in%Dep$seqn,]
s<-Slph[Slph$seqn%in%a$seqn,]
Tot<-merge(a,s,by="seqn")
Tot<-merge(Tot,Dep,by="seqn")
library(survey)
d1 <- svydesign(id=~seqn, weights=~wtint2yr, data=Tot)
cat<-svyby(formula = ~sld012, by = ~dmqmiliz, design = d1, FUN = svymean, na.rm=T)
svychisq(~sld012+dmqmiliz, design = d1)
qplot(x=cat$dmqmiliz,y=cat$sld012, data=cat ,xlab="Veteran", ylab="Sleeping Time" )+
  geom_errorbar(aes(x=dmqmiliz, ymin=sld012-se,ymax= sld012+se), width=.25)+
  ggtitle(label = "Veteran with Sleeping problem")

#calculate race*education*health cross tabulation, and plot it
catdep<-svyby(formula =~sld012, by = ~dmqmiliz+dpq020, design = d1, FUN = svymean, na.rm=T)

#this plot is a little more complicated
#catdep$Vet<-rep(c("Veteran","Non-Veteran"),3)
p<-ggplot(catdep, aes(dpq020,sld012),xlab="Depression", ylab="Sleeping Time")
p<-p+geom_point(aes(colour=dmqmiliz))
p<-p+geom_line(aes(colour=dmqmiliz,group=dmqmiliz))
p<-p+geom_errorbar(aes(x=dpq020, ymin=sld012-se,ymax= sld012+se,colour=dmqmiliz), width=1)
p<-p+ylab("Sleeping Time")
p<-p+xlab("Depression")
p+ggtitle("Sleeping Time based on Depression Level")

#calculate race*education*health cross tabulation, and plot it
catslp<-svyby(formula =~sld012, by = ~dmqmiliz+dpq030, design = d1, FUN = svymean, na.rm=T)

#this plot is a little more complicated
#catdep$Vet<-rep(c("Veteran","Non-Veteran"),3)
p<-ggplot(catslp, aes(dpq030,sld012),xlab="Depression", ylab="Sleeping Time")
p<-p+geom_point(aes(colour=dmqmiliz))
p<-p+geom_line(aes(colour=dmqmiliz,group=dmqmiliz))
p<-p+geom_errorbar(aes(x=dpq030, ymin=sld012-se,ymax= sld012+se,colour=dmqmiliz), width=1)
p<-p+ylab("Sleeping Time")
p<-p+xlab("Sleeping Problems")
p+ggtitle("Sleeping Time based on Sleeping Problem")


plot(svytable(~dpq040+dmqmiliz,d1))
summary(svyglm(dpq040~dmqmiliz, design=d1))
summary(svyglm(sld012~dmqmiliz, design=d1))

##Mental Health
Drug<- sasxport.get("C:/Users/Umit Tokac/OneDrive/Documents/TT/Research/Publications/NHANES/DrugUSE.XPT")
Med<- sasxport.get("C:/Users/Umit Tokac/OneDrive/Documents/TT/Research/Publications/NHANES/MedicalCon.XPT")
Alch<- sasxport.get("C:/Users/Umit Tokac/OneDrive/Documents/TT/Research/Publications/NHANES/Alcoholuse.XPT")
##Let's merge all of the data 
M<-M[M$seqn%in%Tot_d$seqn,]
Tot.dat<-merge(Tot_d,M,by="seqn")
alch<-Alch[Alch$seqn%in%Tot.dat$seqn,]
Tot.dat<-merge(Tot.dat,alch,by="seqn")
d2 <- svydesign(id=~sdmvpsu, strata =~sdmvstra, weights=~wtint2yr, data=Tot.dat, nest=TRUE)
library(gmodels)
CrossTable(Tot.dat$dmqmiliz,Tot.dat$duq240,chisq=TRUE) ###Veterans vs cocaine/heroin/methamphetamine use
svychisq(~dmqmiliz+duq240,d2)
##################
Tot.dat1 <- Tot.dat[ which(Tot.dat$dpq020<4), ]##removing refused and missing values from data
d3 <- svydesign(id=~sdmvpsu, strata =~sdmvstra, weights=~wtint2yr, data=Tot.dat1, nest=TRUE)

svychisq(~dmqmiliz+dpq020,d3)##feeling down, depress and hopeless
summary(svyglm(dpq020~dmqmiliz, design=d3))
summary(svyglm(dpq030~dmqmiliz, design=d2))##Sleeping problems
summary(svyglm(dpq040~dmqmiliz, design=d2))##Feeling tired
summary(svyglm(sld012~dmqmiliz, design=d2))
##########################
CrossTable(Tot.dat$dmqmiliz,Tot.dat$duq272) ### How often use cocaine/heroin/methamphetamine
CrossTable(Tot.dat$dmqmiliz,Tot.dat$mcq080) ###Veterans vs overweight
CrossTable(Tot.dat$dmqmiliz,Tot.dat$mcq160l) ##liver problem
#CrossTable(Tot$dmqmiliz,Tot$indfmpir)
summary(svyglm(indfmpir~dmqmiliz, design=d2))##Veterans houseincome
cat<-svyby(formula = ~indfmpir, by = ~dmqmiliz, design = d2, FUN = svymean, na.rm=T)
qplot(x=cat$dmqmiliz,y=cat$indfmpir, data=cat ,xlab="Veteran Status", ylab="Household Income" )+
  geom_errorbar(aes(x=dmqmiliz, ymin=indfmpir-se,ymax= indfmpir+se), width=.25)+xlim("Non-Veteran","Veteran")

##Veterans alcohol used
summary(svyglm(alq160~dmqmiliz, design=d2))##Veterans alcohol use
#summary(svyglm(alq130~dmqmiliz, design=d2))
summary(svyglm(alq160~dmqmiliz, design=d2))
cat1<-svyby(formula = ~alq160, by = ~dmqmiliz, design = d2, FUN = svymean, na.rm=T)
qplot(x=cat1$dmqmiliz,y=cat1$alq160, data=cat1 ,xlab="Veteran Status", ylab="Alcohol Use" )+
  geom_errorbar(aes(x=dmqmiliz, ymin=alq160-se,ymax= alq160+se), width=.25)+xlim("Non-Veteran","Veteran")