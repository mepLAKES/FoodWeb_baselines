################################################################################
# Title:        Figure 4 : Comparing trophic metrics at the community level 
# Date:         2025-05-05
# Version:      6.0
# Notes:        Any additional information or context
# Dependencies: dplyr, ggplot2, tidyr, SIBER
################################################################################


################################################################################
# 1. SETUP ----------
################################################################################
## Clear environment
rm(list = ls())

## Load required packages


library(dplyr)
library(ggplot2)
library(ggpubr)

pal<-hcl.colors(6,"RdBu")
pal2<-hcl.colors(3,"PiYG")

#files
load("data/processed/dataset_with_metrics_community.RData")
df<-dataset_with_metrics_community 

################################################################################
# 2. DATA PREP ----------
################################################################################


df_obs<-df %>% filter(dataset=="Observed")
colnames(df_obs)[5:15]<-paste0(colnames(df_obs)[5:15],"_obs")

df_pred<-df %>% filter(dataset=="Predicted")
colnames(df_pred)[5:15]<-paste0(colnames(df_pred)[5:15],"_pred")

df_merged<-merge(df_obs,df_pred,by=c("FW_ID","Ecosystem_Type"),all.x=TRUE)[,-c(3,4,16,17)]
df_merged<-df_merged %>% filter(dX_range_obs<=5)

################################################################################
# 3. PLOTS ----------
################################################################################

#panel 1 FCL

x=df_merged$FCL_obs
y=df_merged$FCL_pred
E=df_merged$Ecosystem_Type
dfg<-data.frame (E,x,y)

n=length(!is.na(x))
cor_test<-cor.test(x,y)
r<-cor(x,y,use='pairwise.complete.obs')


Fig_FCL<-ggplot(dfg,aes(x=x,y=y,col=E))+
  geom_point(size=2,alpha=0.4)+
  geom_abline(aes(intercept=0,slope=1),lty="dashed",lwd=1) +#  geom_abline(intercept=0,slope=1)+
  theme_classic()+
 # xlim(-0,1.1)+
  annotate(geom="text",label=paste("r = ",round(cor_test$estimate,digits=2)," p<0.05"),x=6,y=2.5,size=3,fontface = 'italic')+
  annotate(geom="text",label=paste("n = ",n),x=6,y=3,size=3,fontface = 'italic')+
  scale_color_manual(values = c("Lentic" = pal[6], "Lotic" = pal[2]))+
  labs(x = "FCL_Obs (unitless) " ,y = "FCL_Pred (unitless)",col="")+
  theme(legend.position = "top")#+
#  theme(plot.margin = margin(1,0.2,0.2,0.2, "cm"))
Fig_FCL

#++++++++++++++++++++++++++++++++++

#panel 2 TPmean_obs

x=df_merged$TPmean_obs
y=df_merged$TPmean_pred
E=df_merged$Ecosystem_Type
dfg<-data.frame (E,x,y)

n=length(!is.na(x))
cor_test<-cor.test(x,y)
r<-cor(x,y,use='pairwise.complete.obs')


Fig_TPmean<-ggplot(dfg,aes(x=x,y=y,col=E))+
  geom_point(size=2,alpha=0.7)+
  geom_abline(aes(intercept=0,slope=1),lty="dashed",lwd=1) +#  geom_abline(intercept=0,slope=1)+
  theme_classic()+
   xlim(1,5)+ylim(1,6)+
  annotate(geom="text",label=paste("r = ",round(cor_test$estimate,digits=2)," p<0.05"),x=4,y=1,size=3,fontface = 'italic')+
  annotate(geom="text",label=paste("n = ",n),x=4,y=1.5,size=3,fontface = 'italic')+
  scale_color_manual(values = c("Lentic" = pal[6], "Lotic" = pal[2]))+
  labs(x = "mean TP_Obs (unitless)" ,y = "mean TP_Pred (unitless)",col="")+
  theme(legend.position = "top")#+
#  theme(plot.margin = margin(1,0.2,0.2,0.2, "cm"))
Fig_TPmean

#++++++++++++++++++++++++++++++++++

#panel 3 TA

x=df_merged$TA_obs
y=df_merged$TA_pred
E=df_merged$Ecosystem_Type
dfg<-data.frame (E,x,y)

n=length(!is.na(x))
cor_test<-cor.test(x,y)
r<-cor(x,y,use='pairwise.complete.obs')


Fig_TA<-ggplot(dfg,aes(x=x,y=y,col=E))+
  geom_point(size=2,alpha=0.7)+
  geom_abline(aes(intercept=0,slope=1),lty="dashed",lwd=1) +#  geom_abline(intercept=0,slope=1)+
  theme_classic()+
  xlim(0,25)+
  #ylim(1,6)+
  annotate(geom="text",label=paste("r = ",round(cor_test$estimate,digits=2)," p<0.05"),x=20,y=10,size=3,fontface = 'italic')+
  annotate(geom="text",label=paste("n = ",n),x=20,y=12,size=3,fontface = 'italic')+
  scale_color_manual(values = c("Lentic" = pal[6], "Lotic" = pal[2]))+
  labs(x = expression(paste("TA_Obs ","(\u2030)"^2)) ,y = expression(paste("TA_Pred ","(\u2030)"^2)),col="")+
  theme(legend.position = "top")#+
#  theme(plot.margin = margin(1,0.2,0.2,0.2, "cm"))
Fig_TA


#++++++++++++++++++++++++++++++++++

#panel 4 CD

x=df_merged$CD_obs
y=df_merged$CD_pred
E=df_merged$Ecosystem_Type
dfg<-data.frame (E,x,y)

n=length(!is.na(x))
cor_test<-cor.test(x,y)
r<-cor(x,y,use='pairwise.complete.obs')


Fig_CD<-ggplot(dfg,aes(x=x,y=y,col=E))+
  geom_point(size=2,alpha=0.7)+
  geom_abline(aes(intercept=0,slope=1),lty="dashed",lwd=1) +#  geom_abline(intercept=0,slope=1)+
  theme_classic()+
  #  xlim(1,5)+ylim(1,6)+
  annotate(geom="text",label=paste("r = ",round(cor_test$estimate,digits=2)," p<0.05"),x=2.2,y=0.0,size=3,fontface = 'italic')+
  annotate(geom="text",label=paste("n = ",n),x=2.2,y=0.4,size=3,fontface = 'italic')+
  scale_color_manual(values = c("Lentic" = pal[6], "Lotic" = pal[2]))+
  labs(x = "CD_Obs (\u2030)" ,y = "CD_Pred (\u2030)",col="")+
  theme(legend.position = "top")#+
#  theme(plot.margin = margin(1,0.2,0.2,0.2, "cm"))
Fig_CD

#++++++++++++++++++++++++++++++++++

#panel 5 NND

x=df_merged$NND_obs
y=df_merged$NND_pred
E=df_merged$Ecosystem_Type
dfg<-data.frame (E,x,y)

n=length(!is.na(x))
cor_test<-cor.test(x,y)
r<-cor(x,y,use='pairwise.complete.obs')


Fig_NND<-ggplot(dfg,aes(x=x,y=y,col=E))+
  geom_point(size=2,alpha=0.7)+
  geom_abline(aes(intercept=0,slope=1),lty="dashed",lwd=1) +#  geom_abline(intercept=0,slope=1)+
  theme_classic()+
  xlim(0,3)+
  #ylim(1,6)+
  annotate(geom="text",label=paste("r = ",round(cor_test$estimate,digits=2)," p<0.05"),x=2.5,y=1,size=3,fontface = 'italic')+
  annotate(geom="text",label=paste("n = ",n),x=2.5,y=1.5,size=3,fontface = 'italic')+
  scale_color_manual(values = c("Lentic" = pal[6], "Lotic" = pal[2]))+
  labs(x = "NND_Obs (\u2030)" ,y = "NND_Pred (\u2030)",col="")+
  theme(legend.position = "top")#+
#  theme(plot.margin = margin(1,0.2,0.2,0.2, "cm"))
Fig_NND

#++++++++++++++++++++++++++++++++++

#panel 6 SNND

x=df_merged$SDNND_obs
y=df_merged$SDNND_pred
E=df_merged$Ecosystem_Type
dfg<-data.frame (E,x,y)

n=length(!is.na(x))
cor_test<-cor.test(x,y)
r<-cor(x,y,use='pairwise.complete.obs')


Fig_SNND<-ggplot(dfg,aes(x=x,y=y,col=E))+
  geom_point(size=2,alpha=0.7)+
  geom_abline(aes(intercept=0,slope=1),lty="dashed",lwd=1) +#  geom_abline(intercept=0,slope=1)+
  theme_classic()+
  #xlim(0,3)+
  #ylim(1,6)+
  annotate(geom="text",label=paste("r = ",round(cor_test$estimate,digits=2)," p<0.05"),x=2.5,y=0.5,size=3,fontface = 'italic')+
  annotate(geom="text",label=paste("n = ",n),x=2.5,y=1,size=3,fontface = 'italic')+
  scale_color_manual(values = c("Lentic" = pal[6], "Lotic" = pal[2]))+
  labs(x = "SDNND_Obs (\u2030)" ,y = "SDNND_Pred (\u2030)",col="")+
  theme(legend.position = "top")#+
#  theme(plot.margin = margin(1,0.2,0.2,0.2, "cm"))
Fig_SNND

#++++++++++++++++++++++++++++++++++

#panel 7 SEA

x=as.numeric(df_merged$SEA_obs)
y=as.numeric(df_merged$SEA_pred)
E=df_merged$Ecosystem_Type
dfg<-data.frame (E,x,y)

n=length(!is.na(x))
cor_test<-cor.test(x,y)
r<-cor(x,y,use='pairwise.complete.obs')


Fig_SEA<-ggplot(dfg,aes(x=x,y=y,col=E))+
  geom_point(size=2,alpha=0.7)+
  geom_abline(aes(intercept=0,slope=1),lty="dashed",lwd=1) +#  geom_abline(intercept=0,slope=1)+
  theme_classic()+
  #  xlim(1,5)+ylim(1,6)+
  annotate(geom="text",label=paste("r = ",round(cor_test$estimate,digits=2)," p<0.05"),x=6.7,y=4,size=3,fontface = 'italic')+
  annotate(geom="text",label=paste("n = ",n),x=6.7,y=5,size=3,fontface = 'italic')+
  scale_color_manual(values = c("Lentic" = pal[6], "Lotic" = pal[2]))+
  labs(x = expression(paste("SEA_Obs","(\u2030)"^2)) ,y = expression(paste("SEA_Pred","(\u2030)"^2)),col="")+
  theme(legend.position = "top")#+
#  theme(plot.margin = margin(1,0.2,0.2,0.2, "cm"))
Fig_SEA
#++++++++++++++++++++++++++++++++++

#panel 8 SEAc

x=as.numeric(df_merged$SEAc_obs)
y=as.numeric(df_merged$SEAc_pred)
E=df_merged$Ecosystem_Type
dfg<-data.frame (E,x,y)

n=length(!is.na(x))
cor_test<-cor.test(x,y)
r<-cor(x,y,use='pairwise.complete.obs')


Fig_SEAc<-ggplot(dfg,aes(x=x,y=y,col=E))+
  geom_point(size=2,alpha=0.7)+
  geom_abline(aes(intercept=0,slope=1),lty="dashed",lwd=1) +#  geom_abline(intercept=0,slope=1)+
  theme_classic()+
    xlim(0,7.5)+
  ylim(1,6)+
  annotate(geom="text",label=paste("r = ",round(cor_test$estimate,digits=2)," p<0.05"),x=6,y=3,size=3,fontface = 'italic')+
  annotate(geom="text",label=paste("n = ",n),x=6,y=3.5,size=3,fontface = 'italic')+
  scale_color_manual(values = c("Lentic" = pal[6], "Lotic" = pal[2]))+
  labs(x = expression(paste("SEAc_Obs ","(\u2030)"^2)) ,y = expression(paste("SEAc_Pred ","(\u2030)"^2)),col="")+
  theme(legend.position = "top")#+
#  theme(plot.margin = margin(1,0.2,0.2,0.2, "cm"))
Fig_SEAc

jpeg("figures/Fig4.jpeg",width=28,height=14,units="cm",res=300)
ggarrange(Fig_FCL, Fig_TPmean, Fig_CD, Fig_NND, Fig_SNND, Fig_TA, Fig_SEA, Fig_SEAc, ncol = 4, nrow = 2, common.legend = TRUE, legend = "top",labels = c("A", "B","C","D","E","F","G","H"))
dev.off()
