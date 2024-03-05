library(Hmisc)
nhanes <- sasxport.get("C:/Users/tokacu/Downloads/ACQ_G.xpt")
# character variables are converted to R

Demo<- sasxport.get("C:/Users/tokacu/OneDrive/Documents/TT/Research/Publications/NHANES/DEMO_I.XPT")
Dep<- sasxport.get("C:/Users/tokacu/OneDrive/Documents/TT/Research/Publications/NHANES/Depression.XPT")
Sleep<-sasxport.get("C:/Users/tokacu/OneDrive/Documents/TT/Research/Publications/NHANES/SleepingDis.XPT")
Act<-sasxport.get("C:/Users/tokacu/OneDrive/Documents/TT/Research/Publications/NHANES/PhysicalAct.XPT")
Chol<- sasxport.get("C:/Users/tokacu/OneDrive/Documents/TT/Research/Publications/NHANES/Cholesterol.XPT")
Alch<- sasxport.get("C:/Users/tokacu/OneDrive/Documents/TT/Research/Publications/NHANES/Alcoholuse.XPT")
kidney<-sasxport.get("C:/Users/tokacu/OneDrive/Documents/TT/Research/Publications/NHANES/KidneyCond.XPT")
Urkidney<-sasxport.get("C:/Users/tokacu/OneDrive/Documents/TT/Research/Publications/NHANES/Urine_Kidney.XPT")
Cigar<-sasxport.get("C:/Users/tokacu/OneDrive/Documents/TT/Research/Publications/NHANES/SmokingCigarette.XPT")
Tobac<-sasxport.get("C:/Users/tokacu/OneDrive/Documents/TT/Research/Publications/NHANES/SmokingTobacco.XPT")
##Merging the data
a<-Demo[Demo$seqn%in%Dep$seqn,]
s<-Sleep[Sleep$seqn%in%a$seqn,]
Tot<-merge(a,s,by="seqn")
c<-Chol[Chol$seqn%in%a$seqn,]
Tot<-merge(Tot,c,by="seqn")
k<-Urkidney[Urkidney$seqn%in%kidney$seqn,]
kid<-merge(k,kidney,by="seqn")
Tot<-Tot[Tot$seqn%in%kid$seqn,]
Tot<-merge(Tot,kid,by="seqn")


#####################################################
nhanes_s <- sasxport.get("C:/Users/tokacu/Desktop/TT/Research/NINR Workshop/2019 NINR Boot Camp/2019 NINR Boot Camp/Speaker Presentations - Thursday July 18/1_45 - Breakout - Fakhouri/SLQ_H.XPT")
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
