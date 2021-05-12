##Load packages
library(ggplot2)
library(dplyr)
library(data.table)


##Load data
mydata.short<-read.csv("data for figure 7.csv")

##Subset for regressions
storm<-subset(mydata.short, event=="Storm")
dredge<-subset(mydata.short, event=="SD")

##Plot SRP vs BAP
ggplot(mydata.short, aes(x=srp, y=bap))+
  geom_point(aes(fill=id),size=4, pch=21)+
  scale_fill_brewer(palette = "PuOr")+
  geom_abline(intercept = 0, slope = 1, linetype="dashed", size=1.2, color="gray37")+
  theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(axis.title.x=element_text(size=18, color="black"))+
  theme(axis.title.y=element_text(size=18, color="black"))+
  theme(axis.text.y=element_text(size=18, color="black"))+
  theme(axis.text.x=element_text(size=18, color="black"))+
  ylab(expression(paste('BAP ('*mu*g~L^-1*')')))+
  xlab(expression(paste('SRP ('*mu*g~L^-1*')')))+
  labs(color="Disturbance")+
  theme(legend.position="top",legend.title=element_blank(),legend.text = element_text(size=14))+
 scale_y_log10()+
  scale_x_log10()

##Linear regression for SRP vs TP
#storm
model<-lm(bap~srp, data=storm)
summary(model)
#dredge
model<-lm(bap~srp, data=dredge)
summary(model)


##Plot SRP vs BAP
ggplot(mydata.short, aes(x=bap, y=tp))+
  geom_point(aes(fill=id),size=4, pch=21)+
  scale_fill_brewer(palette = "PuOr")+
  geom_abline(intercept = 0, slope = 1, linetype="dashed", size=1.2)+
  geom_smooth(method="lm", color="black", se=F)+
  theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(axis.title.x=element_text(size=18, color="black"))+
  theme(axis.title.y=element_text(size=18, color="black"))+
  theme(axis.text.y=element_text(size=18, color="black"))+
  theme(axis.text.x=element_text(size=18, color="black"))+
  xlab(expression(paste('BAP ('*mu*g~L^-1*')')))+
  ylab(expression(paste('TP ('*mu*g~L^-1*')')))+
  labs(color="Disturbance")+
  theme(legend.position="top",legend.title=element_blank(),legend.text = element_text(12))+
  scale_y_log10()+#limits=c(0,1700), breaks=seq(0,1700,200), expand=c(0,0))+
  scale_x_log10()#limits=c(0,900), breaks=seq(0,800,200), expand=c(0,0))

#storm
model<-lm(tp~bap, data=storm)
summary(model)
#dredge
model<-lm(tp~bap, data=dredge)
summary(model)


##Plot BAP vs turbidity
ggplot(mydata.short, aes(x=turb, y=bap))+
  geom_point(aes(fill=id),size=4, pch=21)+
  scale_fill_brewer(palette = "PuOr")+
  scale_linetype_manual(values=c("twodash", "solid"))+
  #geom_abline(intercept = 0, slope = 1, linetype="dashed", size=1.2, color="gray37")+
  geom_smooth(aes(linetype=event),method="lm", color="black", size=1.5, se=F)+
  theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(axis.title.x=element_text(size=18, color="black"))+
  theme(axis.title.y=element_text(size=18, color="black"))+
  theme(axis.text.y=element_text(size=18, color="black"))+
  theme(axis.text.x=element_text(size=18, color="black"))+
  ylab(expression(paste('BAP ('*mu*g~L^-1*')')))+
  xlab(expression(paste('Turbidity (NTU)')))+
  labs(fill="Dredging")+
  theme(legend.position="top",legend.title=element_blank(),legend.text = element_text(12))+
  scale_y_log10()+#limits=c(0,1700), breaks=seq(0,1700,200), expand=c(0,0))+
  scale_x_log10()#limits=c(0,900), breaks=seq(0,800,200), expand=c(0,0))

#storm
model<-lm(log(bap)~log(turb), data=storm)
summary(model)

#dredge
model<-lm(bap~turb, data=dredge)
summary(model)


##Plot SRP vs TP
ggplot(mydata.short, aes(x=srp, y=tp))+
  geom_point(aes(fill=id),size=4, pch=21)+
  scale_fill_brewer(palette = "PuOr")+
  scale_linetype_manual(values=c("twodash", "solid"))+
  geom_abline(intercept = 0, slope = 1, linetype="dashed", size=1.2, color="gray37")+
  geom_smooth(aes(linetype=event),method="lm", color="black", size=1.5, se=F)+
  theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(axis.title.x=element_text(size=18, color="black"))+
  theme(axis.title.y=element_text(size=18, color="black"))+
  theme(axis.text.y=element_text(size=18, color="black"))+
  theme(axis.text.x=element_text(size=18, color="black"))+
  xlab(expression(paste('SRP ('*mu*g~L^-1*')')))+
  ylab(expression(paste('TP ('*mu*g~L^-1*')')))+
  labs(color="Disturbance")+
  theme(legend.position="top",legend.title=element_blank(),legend.text = element_text(14))+
  scale_y_log10()+#limits=c(0,1700), breaks=seq(0,1700,200), expand=c(0,0))+
 scale_x_log10()#limits=c(0,900), breaks=seq(0,800,200), expand=c(0,0))

#storm
model<-lm(tp~srp, data=storm)
summary(model)

#dredge
model<-lm(tp~srp, data=dredge)
summary(model)

