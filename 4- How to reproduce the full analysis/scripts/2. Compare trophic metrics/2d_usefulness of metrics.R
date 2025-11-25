

################################################################################
# Title:        Usefulness of predicted baselines , with figure
# Description:  This scripts provide resultsand figures for metrics comparisons at species, pop and community levels
# Date:         2025-09-05
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



# SET UP -------------------
## Libraries
library(dplyr)
library(ggplot2)
library(tidyr)
library(rprojroot)
library(ggpubr)
library(irr)
##############

## Set the root directory for the project
root.dir = find_rstudio_root_file()
data.dir = paste0(root.dir,'/data')
script.dir = paste0(root.dir,'scripts')
figures.dir = paste0(root.dir,'/figures')
models.dir = paste0(root.dir,'/models')
setwd(root.dir)

## load datasets
load(paste0(data,"/processed/dataset_with_metrics_community.RData"))
load(paste0(data,"/processed/species_dataset_with_metrics.RData"))

head(species_dataset_with_metrics)
head(dataset_with_metrics_community)
###############################

# DATA WRANGLING ------------------- 
## Species level
## Select the nine most represented species 
df_species<-species_dataset_with_metrics %>%
  select(FW_ID,Latin_name,unw_TP_fixed,w_TP_fixed,alpha_benthic_fixed,dataset)%>%
  group_by(Latin_name,dataset,FW_ID)%>%
  summarise(TP=median(unw_TP_fixed,na.rm=TRUE),TPw=median(w_TP_fixed,na.rm=TRUE),alpha_benthic=median(alpha_benthic_fixed,na.rm=TRUE))


  df_species_observed<-df_species %>%
  filter(dataset=="Observed")

X<-sort(table(df_species_observed$Latin_name),decreasing=TRUE)[1:10]
to_keep<-rownames(X)
df_species<-df_species %>%
  filter(Latin_name %in% to_keep)
###############################

# WORKING AT POP LEVELS - WRANGLING -------------------
## comparision for all 10 species _ ordering
### the complete TP table according species of descending TP 
order_species<-df_species %>%
  filter(dataset=="Observed")%>%
  group_by(Latin_name) %>%
  summarise(med_TP=median(TP,na.rm=TRUE)) %>%
  arrange(med_TP) %>%
  pull(Latin_name) 

df_species$Latin_name_2<-factor(df_species$Latin_name,levels=order_species)

## examples of species with different TP ranges
### use "Salmo trutta" as the species with a large TP range 
### and "Cyprinus" for the small range
### and esox as top predator

df_species_large <- df_species %>%
  filter(Latin_name == "Salmo trutta")  %>%
  select(FW_ID,dataset,TP)

df_species_small <- df_species %>%
  filter(Latin_name == "Cyprinus carpio")  %>%
  select(FW_ID,dataset,TP)

df_species_top <- df_species %>%
  filter(Latin_name == "Esox lucius")  %>%
  select(FW_ID,dataset,TP)


### the species-specific TP tables according to sites of descending TP 
order_species_large<-df_species_large %>%
  filter(dataset=="Observed")%>%
  mutate(order=rank(TP))

U<-merge.data.frame(df_species_large,order_species_large,by="FW_ID") 
  
df_species_large<- U %>%
  select(FW_ID,
         Latin_name=Latin_name.x,
         dataset=dataset.x,
         TP=TP.x,
         order)
  

order_species_small<-df_species_small %>%
  filter(dataset=="Observed")%>%
  mutate(order=rank(TP))

V<-merge.data.frame(df_species_small,order_species_small,by="FW_ID")

df_species_small<- V %>%
  select(FW_ID,
         Latin_name=Latin_name.x,
         dataset=dataset.x,
         TP=TP.x,
         order)


order_species_top<-df_species_top %>%
  filter(dataset=="Observed")%>%
  mutate(order=rank(TP))

W<-merge.data.frame(df_species_top,order_species_top,by="FW_ID")

df_species_top<- W %>%
  select(FW_ID,
         Latin_name=Latin_name.x,
         dataset=dataset.x,
         TP=TP.x,
         order)



# WORKING AT POP LEVELS - STATISTICAL TESTS -------------------
## distribution comparisons (T-test, Wilcoxon, KS test)
### Compare distribution of species TP
results_species_TP<-data.frame(
  species=to_keep,
  p_value_test_t=rep(99,10),
  p_value_test_wilcox=rep(99,10),
  p_value_test_ks=rep(99,10)
)


for (i in 1:10) {
  sp<-to_keep[i]
  dfsp<-df_species %>%
    filter(Latin_name==sp) %>%
    select(dataset,TP)
  t<-t.test(TP ~ dataset, data = dfsp)
  results_species_TP$p_value_test_t[i]<-round(t$p.value,2)
  wilcox<-wilcox.test(TP ~ dataset, data = dfsp)
  results_species_TP$p_value_test_wilcox[i]<-round(wilcox$p.value,2)
  ks<-ks.test(TP ~ dataset, data = dfsp)
  results_species_TP$p_value_test_ks[i]<-round(ks$p.value,3)
}


## Compare distribution of species alpha
results_species_alpha<-data.frame(
  species=to_keep,
  p_value_test_t=rep(99,10),
  p_value_test_wilcox=rep(99,10),
  p_value_test_ks=rep(99,10)
)


for (i in 1:10) {
  sp<-to_keep[i]
  dfsp<-df_species %>%
    filter(Latin_name==sp) %>%
    select(dataset,alpha_benthic) %>%
    filter(alpha_benthic >= 0 & alpha_benthic<=1) # to avoid Inf values
  t<-t.test(alpha_benthic ~ dataset, data = dfsp)
  results_species_alpha$p_value_test_t[i]<-round(t$p.value,2)
  wilcox<-wilcox.test(alpha_benthic ~ dataset, data = dfsp)
  results_species_alpha$p_value_test_wilcox[i]<-round(wilcox$p.value,2)
  ks<-ks.test(alpha_benthic ~ dataset, data = dfsp)
  results_species_alpha$p_value_test_ks[i]<-round(ks$p.value,2)
  
  }

## Compare distribution of species TPw
results_species_TPw<-data.frame(
  species=to_keep,
  p_value_test_t=rep(99,10),
  p_value_test_wilcox=rep(99,10),
  p_value_test_ks=rep(99,10)
)


for (i in 1:10) {
  sp<-to_keep[i]
  dfsp<-df_species %>%
    filter(Latin_name==sp) %>%
    select(dataset,TPw)%>%
    filter(TPw >= 0 & TPw<=10) # to avoid Inf values
  t<-t.test(TPw ~ dataset, data = dfsp)
  results_species_TPw$p_value_test_t[i]<-round(t$p.value,2)
  wilcox<-wilcox.test(TPw ~ dataset, data = dfsp)
  results_species_TPw$p_value_test_wilcox[i]<-round(wilcox$p.value,2)
  ks<-ks.test(TPw ~ dataset, data = dfsp)
  results_species_TPw$p_value_test_ks[i]<-round(ks$p.value,2)
  
}

## ranking comparisons (concordance Kendall's W)
# Ranking tests _ Kendall's W
# population metrics
Results_kendall_species_TP<-data.frame(
  species=to_keep,
  kendall_W=rep(99,10),
  p_value=rep(99,10)
)

for (i in 1:10) {
  sp<-to_keep[i]
  dfsp<-df_species %>%
    filter(Latin_name == sp)  %>%
    select(FW_ID,TP,dataset)%>%
    pivot_wider(names_from = dataset, values_from = TP) %>%
    mutate(observed_rank=rank(Observed),predicted_rank=rank(Predicted))%>%
    select(observed_rank,predicted_rank)
  kendall_res<-kendall(dfsp)
  Results_kendall_species_TP$kendall_W[i]<-round(kendall_res$value,2)
  Results_kendall_species_TP$p_value[i]<-round(kendall_res$p.value,4)
}


Results_kendall_species_alpha<-data.frame(
  species=to_keep,
  kendall_W=rep(99,10),
  p_value=rep(99,10)
)

for (i in 1:10) {
  sp<-to_keep[i]
  dfsp<-df_species %>%
    filter(Latin_name == sp)  %>%
    select(FW_ID,alpha_benthic,dataset)%>%
    pivot_wider(names_from = dataset, values_from = alpha_benthic) %>%
    mutate(observed_rank=rank(Observed),predicted_rank=rank(Predicted))%>%
    select(observed_rank,predicted_rank)
  kendall_res<-kendall(dfsp)
  Results_kendall_species_alpha$kendall_W[i]<-round(kendall_res$value,2)
  Results_kendall_species_alpha$p_value[i]<-round(kendall_res$p.value,4)
}

#Results_kendall_species_TPw<-data.frame(
#  species=to_keep,
 # kendall_W=rep(99,10),
#  p_value=rep(99,10)
#)

#for (i in 1:10) {
#  sp<-to_keep[i]
#  dfsp<-predicted_observed_metrics %>%
#    filter(Latin_name == sp)  %>%
#    select(FW_ID,TPw_observed=TP.x,TPw_predicted=TP.y)%>%
#    group_by(FW_ID) %>%
 #   summarise(TPw_observed=mean(TPw_observed,na.rm=TRUE),
#              TPw_predicted=mean(TPw_predicted,na.rm=TRUE))%>%
#    mutate(observed_rank=rank(TPw_observed),predicted_rank=rank(TPw_predicted))%>%
#    select(observed_rank,predicted_rank)
#  kendall_res<-kendall(dfsp)
#  Results_kendall_species_TPw$kendall_W[i]<-round(kendall_res$value,2)
#  Results_kendall_species_TPw$p_value[i]<-round(kendall_res$p.value,4)
#}





# WORKING AT POP LEVELS - GRAPHS -------------------

# Plot TP for the 10 most represented species
#cols <- c("Observed" = "#FF6666", "Predicted" = "#3399CC", "Observed - variable TF" = "#FF6666")
cols <- c("Observed" = "#FF6666", "Predicted" = "#3399CC")

G1<-ggplot(df_species,aes(x=TP,fill=factor(dataset)))+
  geom_density(alpha=0.6)+
  theme_classic()+
  facet_wrap(~ Latin_name_2, ncol = 1,scales="free_y") +
  #    facet_grid(~ Latin_name_2 ~ .) +
  labs(x="TP",y="Density",fill="Baselines")+
  theme(axis.text.x = element_text(vjust=.5, hjust=1),
        axis.text.y = element_text(face="italic",vjust=.5, hjust=1),
        strip.text = element_text(size = 9, face="italic"),
        strip.background = element_blank(),
        #      legend.position="top",
        legend.position="none",
        legend.text = element_text(size=9)) +
  xlim(2,5)+
  # scale_colour_manual(values = cols)+
  scale_fill_manual(values = cols)+ 
  scale_y_discrete()+
  guides(color=guide_legend(nrow=2, byrow=TRUE))


G1
G2<-ggplot(df_species_small,aes(y=TP,x=order,col=dataset,fill=dataset))+
  #  geom_point(alpha=0.6)+
  theme_classic()+
  labs(y="TP",x="Sites Index",colour="Baselines")+
  theme(axis.text.x=element_blank(),
        legend.text = element_text(size=9),
        legend.position="top")+
  #  scale_colour_manual(values = cols)+ylim(c(2,7.5))+
  annotate(geom="text",label="Cyprinus carpio",x=30,y=5,size=3,fontface="italic",hjust=1)+
  #  guides(color=guide_legend(nrow=1, byrow=TRUE))
  scale_colour_manual(name="Baselines",
                      labels = c("Observed","Predicted" ),
                      values=c("#FF6666" ,"#3399CC"))+
  # scale_shape_manual(name="Baselines",
  #                    labels = c("Observed","Predicted") ,
  #                    values=list("",""))+
  scale_fill_manual(name="Baselines",
                    labels = c("Observed","Predicted" ),
                    values=c("#FF6666" ,"#3399CC"))+
  geom_smooth(method='loess',se=TRUE)+
  guides(color=guide_legend(nrow=1, byrow=TRUE))
G2
G3<-ggplot(df_species_large,aes(y=TP,x=order,col=dataset,fill=dataset))+
  #  geom_point(alpha=0.6)+
  theme_classic()+
  labs(y="TP",x="Sites Index",colour="Baselines")+
  theme(axis.text.x=element_blank(),
        legend.text = element_text(size=9),
        legend.position="top")+
  #  scale_colour_manual(values = cols)+
  #  ylim(c(2,6))+
  annotate(geom="text",label="Salmo trutta",x=22.5,y=5,size=3,fontface="italic",hjust=1)+
  # guides(color=guide_legend(nrow=1, byrow=TRUE))
  scale_colour_manual(name="Baselines",
                      labels = c("Observed","Predicted" ),
                      values=c("#FF6666" ,"#3399CC"))+
  scale_fill_manual(name="Baselines",
                    labels = c("Observed","Predicted" ),
                    values=c("#FF6666" ,"#3399CC"))+
  #  scale_shape_manual(name="Baselines",
  #                    labels = c("Observed","Predicted") ,
  #                     values=list("",""))+
  geom_smooth(method='loess',se=TRUE)+
  guides(color=guide_legend(nrow=1, byrow=TRUE))

G3

G4<-ggplot(df_species_top,aes(y=TP,x=order,col=dataset,fill=dataset))+
  #  geom_point(alpha=0.6)+
  theme_classic()+
  labs(y="TP",x="Sites Index",colour="Baselines")+
  theme(axis.text.x=element_blank(),
        legend.text = element_text(size=9),
        legend.position="top")+
  #  scale_colour_manual(values = cols)+ylim(c(2,7.5))+
  annotate(geom="text",label="Esox lucius",x=27,y=5,size=3,fontface="italic",hjust=1)+
  # guides(color=guide_legend(nrow=1, byrow=TRUE))
  scale_colour_manual(name="Baselines",
                      labels = c("Observed","Predicted" ),
                      values=c("#FF6666" ,"#3399CC"))+
  scale_fill_manual(name="Baselines",
                    labels = c("Observed","Predicted" ),
                    values=c("#FF6666" ,"#3399CC"))+
  # scale_shape_manual(name="Baselines",
  #                    labels = c("Observed","Predicted") ,
  #                    values=list("",""))+
  geom_smooth(method='loess',se=TRUE)+
  guides(color=guide_legend(nrow=1, byrow=TRUE))
G4


#############################################################
# WORKING AT COMMUNITY LEVELS - WRANGLING -------------------

### create variables to order the communiy metrics 

## Community metrics
df_community<- dataset_with_metrics_community 
df_community$SEA<-as.numeric(df_community$SEA)


order_FCL<-df_community %>%
  filter(dataset=="Observed")%>%
  arrange(desc(FCL)) %>%
  pull(FW_ID)%>%
  unique()


order_CD<-df_community %>%
  filter(dataset=="Observed")%>%
  arrange(desc(CD)) %>%
  pull(FW_ID)%>%
  unique()

order_SEA<-df_community %>%
  filter(dataset=="Observed")%>%
  arrange(desc(SEA)) %>%
  pull(FW_ID)%>%
  unique()

order_TA<-df_community %>%
  filter(dataset=="Observed")%>%
  arrange(desc(TA)) %>%
  pull(FW_ID)%>%
  unique()

df_community$FW_ID_FCL<-factor(df_community$FW_ID,levels=order_FCL)
df_community$FW_ID_CD<-factor(df_community$FW_ID,levels=order_CD)
df_community$FW_ID_SEA<-factor(df_community$FW_ID,levels=order_SEA)
df_community$FW_ID_TA<-factor(df_community$FW_ID,levels=order_TA)

df_community$site_order_FCL<-as.numeric(df_community$FW_ID_FCL)
df_community$site_order_CD<-as.numeric(df_community$FW_ID_CD)
df_community$site_order_SEA<-as.numeric(df_community$FW_ID_SEA)
df_community$site_order_TA<-as.numeric(df_community$FW_ID_TA)







# PLOTS________________________


G5<-ggplot(df_community,aes(y=FCL,x=site_order_FCL,colour=dataset,fill=dataset))+
#  geom_point(alpha=0.4)+
  theme_classic()+
#  ylim(c(2,6))+
  labs(y="FCL",x="Sites Index",colour="Baselines")+
  theme(axis.text.x=element_blank(),
        #       plot.margin = unit(c(3,1,4,1), "cm"),
        legend.position="top")+
  scale_colour_manual(name="Baselines",
                      labels = c("Observed","Predicted" ),
                      values=c("#FF6666" ,"#3399CC"))+
  scale_fill_manual(name="Baselines",
                    labels = c("Observed","Predicted" ),
                    values=c("#FF6666" ,"#3399CC"))+
#  scale_shape_manual(name="Baselines",
#                     labels = c("Observed","Predicted") ,
#                     values=list("",""))+
  geom_smooth(method='loess',se=TRUE)+
  guides(color=guide_legend(nrow=1, byrow=TRUE))
G5
G6<-ggplot(df_community,aes(y=CD,x=site_order_CD,colour=dataset,fill=dataset))+
 # geom_point(alpha=0.4)+
  theme_classic()+
#  ylim(c(0,4))+
  labs(y="CD",x="Sites Index",colour="Baselines")+
  theme(axis.text.x=element_blank(),
        #       plot.margin = unit(c(3,1,4,1), "cm"),
        legend.position="top")+
  scale_colour_manual(name="Baselines",
                      labels = c("Observed","Predicted" ),
                      values=c("#FF6666" ,"#3399CC"))+
  scale_fill_manual(name="Baselines",
                    labels = c("Observed","Predicted" ),
                    values=c("#FF6666" ,"#3399CC"))+
  scale_shape_manual(name="Baselines",
                     labels = c("Observed","Predicted") ,
                     values=list("",""))+
  geom_smooth(method='loess',se=TRUE)+
  guides(color=guide_legend(nrow=1, byrow=TRUE))
G6

G7<-ggplot(df_community,aes(y=SEA,x=site_order_SEA,colour=dataset,fill=dataset))+
#  geom_point(alpha=0.4)+
  theme_classic()+
#ylim(c(0,10))+
  labs(y="SEA",x="Sites Index",colour="Baselines")+
  theme(axis.text.x=element_blank(),
        #       plot.margin = unit(c(3,1,4,1), "cm"),
        legend.position="top")+
  scale_colour_manual(name="Baselines",
                      labels = c("Observed","Predicted" ),
                      values=c("#FF6666" ,"#3399CC"))+
  scale_fill_manual(name="Baselines",
                    labels = c("Observed","Predicted" ),
                    values=c("#FF6666" ,"#3399CC"))+
  scale_shape_manual(name="Baselines",
                     labels = c("Observed","Predicted") ,
                     values=list("",""))+
  geom_smooth(method='loess',se=TRUE)+
  guides(color=guide_legend(nrow=1, byrow=TRUE))
G7




G5_7<-ggarrange(G5,G6,G7,ncol=1,labels=c("E","F","G"),
                common.legend=TRUE,legend="top", align = "v")
G2_4<-ggarrange(G2,G3,G4,ncol=1,labels=c("B","C","D"),common.legend=TRUE)

G1_4<-ggarrange(G1,G2_4,ncol=2,labels=c("A",""),widths=c(2,2))

G_f<-ggarrange(G1_4,G5_7,ncol=2,labels=c("",""),widths=c(4,2))

jpeg(paste0(figures.dir,"/Figure_5.jpg"),width=10,height=9,units="in",res=300)
G_f
dev.off()

