
################################################################################
# Title:         - Figure 2 - Goodness of fit of the models 
# Date:         2025-09-05
# Version:      6.0
# Notes:        Any additional information or context
################################################################################


################################################################################
# 1. SETUP ----------
################################################################################
## Clear environment
rm(list = ls())

## Load required packages#figures for papers
library(ggplot2)
library(ggpubr)
library(maps)
library(mapdata)
library(dplyr)
library(ggrepel)
library(grid)
library(gridExtra)

#files
load(file="data/processed/df_test_train_C_lotic.RData")
df_C_lotic<-df_test_train
load(file="data/processed/df_test_train_N_lotic.RData")
df_N_lotic<-df_test_train
load(file="data/processed/df_test_train_C_lentic.RData")
df_C_lentic<-df_test_train
load(file="data/processed/df_test_train_N_lentic.RData")
df_N_lentic<-df_test_train

#colors
pal<-hcl.colors(6, "RdBu")

################################################################################
# 2.COMPUTE CORRELATION FOR TEXTS ON FIGURES
################################################################################

r_C_Lotic<-round(cor(df_C_lotic$obs_values,df_C_lotic$pred_val,use="complete.obs"),digits=2)
range(df_C_lotic$obs_values)
range(df_C_lotic$pred_val,na.rm=TRUE)

r_N_Lotic<-round(cor(df_N_lotic$obs_values,df_N_lotic$pred_val,use="complete.obs"),digits=2)
r_C_Lentic<-round(cor(df_C_lentic$obs_values,df_C_lentic$pred_val,use="complete.obs"), digits=2)
r_N_Lentic<-round(cor(df_N_lentic$obs_values,df_N_lentic$pred_val,use="complete.obs"),digits=2)



Gmod13C_lentic<-ggplot(df_C_lentic,aes(x=obs_values,y=pred_val,col=Hab))+
  geom_point(size=0.7,alpha=0.7)+theme_classic()+
  scale_color_manual(values=c(pal[6],pal[5]))+
  #  geom_abline (slope=1, linetype = "dashed", color="black")+
  annotate(geom="text",label=paste("r = ",r_C_Lentic),x=-15,y=-35,size=2)+
    xlim(-42,-10)+ylim(-36,-15)+
  labs(x = expression(paste("Observed ",delta^13,"C (\u2030)")),y = expression(paste("Predicted ",delta^13,"C (\u2030)")),col="")+
  theme(legend.position = "top",text = element_text(size = 8),title = element_text(size = 8),legend=element_text(size=6))

Gmod15N_lentic<-ggplot(df_N_lentic,aes(x=obs_values,y=pred_val,col=Hab))+
  geom_point(size=0.7,alpha=0.7)+theme_classic()+
  scale_color_manual(values=c(pal[6],pal[5]))+
  #  geom_abline (slope=1, linetype = "dashed", color="black")+
  annotate(geom="text",label=paste("r = ",r_N_Lentic),x=16,y=-0.5,size=2)+
  xlim(-2,20)+ylim(-2,17)+
  labs(x = expression(paste("Observed ",delta^15,"N (\u2030)")),y = expression(paste("Predicted ",delta^15,"N (\u2030)")),col="")+
  theme(legend.position = "top",text = element_text(size = 8),title = element_text(size = 8),legend=element_text(size=6))

Gmod13C_lotic<-ggplot(df_C_lotic,aes(x=obs_values,y=pred_val,col=Hab))+
  geom_point(size=0.7,alpha=0.7)+theme_classic()+
  scale_color_manual(values=c(pal[1],pal[2]))+  
  #  geom_abline (slope=1, linetype = "dashed", color="black")+
  annotate(geom="text",label=paste("r = ",r_C_Lotic),x=-18,y=-35,size=2)+
  xlim(-42,-10)+ylim(-36,-15)+
  labs(x = expression(paste("Observed ",delta^13,"C (\u2030)")),y = expression(paste("Predicted ",delta^13,"C (\u2030)")),col="")+
  theme(legend.position = "top",text = element_text(size = 8),title = element_text(size = 8),legend=element_text(size=6))

Gmod15N_lotic<-ggplot(df_N_lotic,aes(x=obs_values,y=pred_val,col=Hab))+
  geom_point(size=0.7,alpha=0.7)+theme_classic()+
  scale_color_manual(values=c(pal[1],pal[2]))+
  #  geom_abline (slope=1, linetype = "dashed", color="black")+
  xlim(-2,20)+ylim(-2,17)+
  annotate(geom="text",label=paste("r = ",r_N_Lotic),x=16,y=-1,size=2)+
  labs(x = expression(paste("Observed ",delta^15,"N (\u2030)")),y = expression(paste("Predicted ",delta^15,"N (\u2030)")),col="")+
  theme(legend.position = "top",text = element_text(size = 8),title = element_text(size = 8),legend=element_text(size=6))

Fig2a<-ggarrange(Gmod13C_lentic,Gmod15N_lentic,nrow=2,labels=c("A","C"),font.label = list(size = 9, color = "black", face = "bold", family = NULL),common.legend = TRUE)
Fig2b<-ggarrange(Gmod13C_lotic,Gmod15N_lotic,nrow=2,labels=c("B","D"),font.label = list(size = 9, color = "black", face = "bold", family = NULL),common.legend = TRUE)
Fig2<-ggarrange(Fig2a,Fig2b,ncol=2)
Fig2

jpeg("figures/Fig2.jpg",width=9,height=9,units="cm",res=800)

Fig2
dev.off()

