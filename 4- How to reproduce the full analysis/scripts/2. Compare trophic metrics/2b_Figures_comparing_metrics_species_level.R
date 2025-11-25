################################################################################
# Title:        Figure 3 : Comparing trophic metrics at the species level 
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
load( file="data/processed/species_dataset_with_metrics.RData")

################################################################################
# 2. DATA PREP ----------
################################################################################

df_obs<-species_dataset_with_metrics %>% select (
  FW_ID,
  Latin_name,
  Ecosystem_Type,
  dataset,
  d13C,
  d13C_cor,
  d15N_cor,
  alpha_benthic_fixed_obs=alpha_benthic_fixed,
  alpha_benthic_var_obs=alpha_benthic_var,
  w_TP_fixed_obs=w_TP_fixed,
  w_TP_var_obs=w_TP_var,
  unw_TP_fixed_obs=unw_TP_fixed,
  unw_TP_var_obs=unw_TP_var) %>% 
  filter (dataset=="Observed")

df_pred<-species_dataset_with_metrics %>% select (
  FW_ID,
  Latin_name,
  Ecosystem_Type,
  dataset,
  d13C,
  d13C_cor,
  d15N_cor,
  alpha_benthic_fixed_pred=alpha_benthic_fixed,
  alpha_benthic_var_pred=alpha_benthic_var,
  w_TP_fixed_pred=w_TP_fixed,
  w_TP_var_pred=w_TP_var,
  unw_TP_fixed_pred=unw_TP_fixed,
  unw_TP_var_pred=unw_TP_var) %>% 
  filter (dataset=="Predicted")

df_all<-merge(df_obs, df_pred, by=c("FW_ID","Latin_name","d13C","Ecosystem_Type")) 

df_all$error_baseline_alpha<-df_all$alpha_benthic_fixed_obs-df_all$alpha_benthic_fixed_pred
df_all$error_frac_alpha<-df_all$alpha_benthic_fixed_obs-df_all$alpha_benthic_var_obs
df_all$error_all_alpha<-df_all$alpha_benthic_fixed_obs-df_all$alpha_benthic_var_pred

df_all$error_baseline_unw_TP<-df_all$w_TP_fixed_obs-df_all$w_TP_fixed_pred
df_all$error_frac_unw_TP<-df_all$w_TP_fixed_obs-df_all$w_TP_var_obs
df_all$error_all_unw_TP<-df_all$w_TP_fixed_obs-df_all$w_TP_var_pred

df_all$error_baseline_w_TP<-df_all$w_TP_fixed_obs-df_all$w_TP_fixed_pred
df_all$error_frac_w_TP<-df_all$w_TP_fixed_obs-df_all$w_TP_var_obs
df_all$error_all_w_TP<-df_all$w_TP_fixed_obs-df_all$w_TP_var_pred


################################################################################
# 3. PLOTS ----------
################################################################################

## panel 1 alpha_benthic_fixed
df<-df_all %>% filter (alpha_benthic_fixed_obs>=0 & alpha_benthic_fixed_obs<=1)
n=length(!is.na(df$alpha_benthic_fixed_obs))
cor_test<-cor.test(df$alpha_benthic_fixed_obs,df$alpha_benthic_fixed_pred)
r<-cor(df$alpha_benthic_fixed_obs,df$alpha_benthic_fixed_pred,use='pairwise.complete.obs')
label <- paste0(
  "r == ", round(cor_test$estimate, 2),
  " * ',' ~ p < 10^{-15}"
)

TP1<-ggplot(df,aes(x=alpha_benthic_fixed_obs,y=alpha_benthic_fixed_pred,col=Ecosystem_Type))+
  geom_point(size=2,alpha=0.7)+
 geom_abline(aes(intercept=0,slope=1),lty="dashed",lwd=1) +#  geom_abline(intercept=0,slope=1)+
  theme_classic()+
  xlim(-0,1.1)+ylim(-6,4)+
  annotate(geom="text",label=label,parse=TRUE,x=0.85,y=-4.8,size=3,fontface = 'italic')+
  annotate(geom="text",label=paste("n = ",n),x=0.85,y=-5.8,size=3,fontface = 'italic')+
  scale_color_manual(values = c("Lentic" = pal[6], "Lotic" = pal[2]))+
  labs(x = expression(paste(alpha[benthic],"from observed baselines" )),y = expression(paste(alpha[benthic],"from simulated baselines" )),col="")+
  theme(legend.position = "top")#+
#  theme(plot.margin = margin(1,0.2,0.2,0.2, "cm"))
TP1
m<-nrow(df)
test_df<-data.frame(error=c(df$error_baseline_alpha,df$error_frac_alpha,df$error_all_alpha),type=c(rep("Baseline",m),rep("Fractionation",m),rep("Total",m)))

TP1b<-ggplot(test_df, aes(x=type, y=error, fill=type)) + 
  scale_fill_manual(values = c("Baseline" = pal2[1], "Fractionation" = pal2[3],"Total"=pal2[2]))+
  geom_violin()+
  ylim(-5,5)+
  theme_classic()+
  labs(x = "",y = expression(paste("Error in ", alpha[benthic])),fill="")+theme(legend.position = "none")#+
#  theme(plot.margin = margin(1,0.2,0.2,0.2, "cm"))
TP1b





####################################################
###########################
## panel 2 TP_unweighted
df<-df_all 
n=length(!is.na(df$unw_TP_fixed_obs))
cor_test<-cor.test(df$unw_TP_fixed_obs,df$unw_TP_fixed_pred)
r<-cor(df$unw_TP_fixed_obs,df$unw_TP_fixed_pred,use='pairwise.complete.obs')
label <- paste0(
  "r == ", round(cor_test$estimate, 2),
  " * ',' ~ p < 10^{-15}"
)

TP2<-ggplot(df,aes(x=unw_TP_fixed_obs,y=unw_TP_fixed_pred,col=Ecosystem_Type))+
  geom_point(size=2,alpha=0.7)+
  geom_abline(aes(intercept=0,slope=1),lty="dashed",lwd=1) +#  geom_abline(intercept=0,slope=1)+
  theme_classic()+
 # xlim(-0,1.1)+
  annotate(geom="text",label=label,parse=TRUE,x=6,y=-4,size=3,fontface = 'italic')+
  annotate(geom="text",label=paste("n = ",n),x=6,y=-5,size=3,fontface = 'italic')+
  scale_color_manual(values = c("Lentic" = pal[6], "Lotic" = pal[2]))+
  labs(x = "unweighted TP from observed baselines",y = "unweighted TP from simulated baselines",col="")+
  theme(legend.position = "top")#+
#  theme(plot.margin = margin(1,0.2,0.2,0.2, "cm"))
TP2
m<-nrow(df)
test_df<-data.frame(error=c(df$error_baseline_unw_TP, df$error_frac_unw_TP,df$error_all_unw_TP),type=c(rep("Baseline",m),rep("Fractionation",m),rep("Total",m)))

TP2b<-ggplot(test_df, aes(x=type, y=error, fill=type)) + 
  scale_fill_manual(values = c("Baseline" = pal2[1], "Fractionation" = pal2[3],"Total"=pal2[2]))+
  geom_violin()+
  ylim(-5,5)+
  theme_classic()+
  labs(x = "",y = "Error in unweighted TP",fill="")+theme(legend.position = "none")#+
#  theme(plot.margin = margin(1,0.2,0.2,0.2, "cm"))
TP2b



## panel 3 TP_weighted
df<-df_all %>% filter (alpha_benthic_var_obs>=0 & alpha_benthic_var_obs<=1)
n=length(!is.na(df$w_TP_fixed_obs))
cor_test<-cor.test(df$w_TP_fixed_obs,df$w_TP_fixed_pred)
r<-cor(df$w_TP_fixed_obs,df$w_TP_fixed_pred,use='pairwise.complete.obs')
label <- paste0(
  "r == ", round(cor_test$estimate, 2),
  " * ',' ~ p < 10^{-15}"
)

TP3<-ggplot(df,aes(x=w_TP_fixed_obs,y=w_TP_fixed_pred,col=Ecosystem_Type))+
  geom_point(size=2,alpha=0.7)+
  geom_abline(aes(intercept=0,slope=1),lty="dashed",lwd=1) +#  geom_abline(intercept=0,slope=1)+
  theme_classic()+
  xlim(0,9)+
  annotate(geom="text",label=label,parse=TRUE,x=5.5,y=-4,size=3,fontface = 'italic')+
  annotate(geom="text",label=paste("n = ",n),x=5.5,y=-5,size=3,fontface = 'italic')+
  scale_color_manual(values = c("Lentic" = pal[6], "Lotic" = pal[2]))+
  labs(x = "weighted TP from observed baselines",y = "weighted TP from simulated baselines",col="")+
  theme(legend.position = "top")#+
#  theme(plot.margin = margin(1,0.2,0.2,0.2, "cm"))

m<-nrow(df)
test_df<-data.frame(error=c(df$error_baseline_w_TP, df$error_frac_w_TP,df$error_all_w_TP),type=c(rep("Baseline",m),rep("Fractionation",m),rep("Total",m)))

TP3b<-ggplot(test_df, aes(x=type, y=error, fill=type)) + 
  scale_fill_manual(values = c("Baseline" = pal2[1], "Fractionation" = pal2[3],"Total"=pal2[2]))+
  geom_violin()+
  ylim(-5,5)+
  theme_classic()+
  labs(x = "",y = "Error in weighted TP",fill="")+theme(legend.position = "none")#+
#  theme(plot.margin = margin(1,0.2,0.2,0.2, "cm"))
TP3b

Fig3<-ggarrange(TP1,TP2, TP3,TP1b,TP2b,TP3b,ncol=3,nrow=2,align="v",labels = c("A", "B","C","D","E","F"))
Fig3


jpeg("figures/Fig3.jpg",width=10,height=6.5,units="in",res=300)
Fig3
dev.off()



