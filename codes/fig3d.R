rm(list=ls())
library(readr)
library(ggplot2)
library(dplyr)
library(RColorBrewer)
library(ggsci)
library(ggthemes)
library(lemon)
library(mratios)
###########################
df<-subset(data.frame(read_csv(
  "./data/controlsp70/df0.csv"))
  ,r0==2.4)

df<- df[c("n","ci","days","ctrl","itr")] %>%
  group_by(days,ctrl,itr) %>%
  summarise(n = sum(n, na.rm = TRUE),
            ci = sum(ci, na.rm = TRUE)) 
unique(df$ctrl)
df<-subset(df,ctrl!="schoolclose_30,workclose_70,otherclose_70,travelrestrict_40")
df<-subset(df,ctrl!="schoolclose_30,workclose_70,otherclose_70,travelrestrict_50")

dfbase<-data.frame(read_csv("./data/basenew.csv"))
dfbase<-subset(dfbase,ssl=="low"& r0==2.4& 
                 start =="Wuhan_NA_China")
###

df365<-data.frame(subset(df,days<=365)[c("n","ci","ctrl","itr","days")] %>%
                    group_by(ctrl,itr) %>%
                    filter(days == max(days)))
min(df365$days)
###
dfbase<-data.frame(read_csv("dfbaseall2.csv"))

dfbase<-subset(dfbase,!is.na(ci))

dfbase<-subset(dfbase,ssl=="low"& r0==2.4& 
                 start =="Wuhan_NA_China")

dfbase<- dfbase[c("n","ci","days","ctrl","itr")] %>%
  group_by(days,ctrl,itr) %>%
  summarise(n = sum(n, na.rm = TRUE),
            ci = sum(ci, na.rm = TRUE)) 


dfbase365<-data.frame(subset(dfbase,days<=365)[c("n","ci","ctrl","itr","days")] %>%
                        group_by(ctrl,itr) %>%
                        filter(days == max(days)))
####
dfci<-rbind(df365,dfbase365)
dfci$ci<-dfci$ci/dfci$n
####
uctrl<-setdiff(unique(dfci$ctrl),c("baseline"))

dfci <- subset(dfci, ci!=0)
dfag<-c()
j<-1
while(j<=length(uctrl)){
  
  cfun<-function(x){
    t<-ttestratio(subset(dfci,ctrl==uctrl[x])$ci,
                  subset(dfci,ctrl=="baseline")$ci )
    return(1-as.numeric(c(t$estimate[3],t$conf.int[1],t$conf.int[2])))
  }
  csp<-data.frame(ctrl=uctrl[j],t(cfun(j)))
  dfag<-rbind(dfag,csp)
  j<-j+1
}

names(dfag)[2:4]<-c("ci_mean","low","upper")

dfag$ctrl2<-gsub("[0-9]+|_|\\.","",dfag$ctrl)
#dfag$ctrl2 <- factor(dfag$ctrl2, levels = c("schoolclose","workclose","otherclose","travelrestrict"))
#dfag$ctrlinten<-as.numeric(as.character(gsub("[a-z]+|_","",dfag$ctrl)))
#########

dfag$ctrlinten<-as.numeric(substring(as.character(dfag$ctrl),58,59))
write.csv(dfag,"./paperrev/paperrev1.csv",row.names = FALSE)

dfag$ci_mean<-dfag$ci_mean-0.9
dfag$low<-dfag$low-0.9
dfag$upper<-dfag$upper-0.9
mypal = pal_npg("nrc", alpha = 1)(9)
library("scales")
show_col(mypal)
mypal[3]

t<-ggplot(data= subset(dfag,ctrlinten %in% c(30,40,50,60,70,80)))  +
  
  geom_bar(aes(x=as.factor(ctrlinten),y = ci_mean ,
               fill= as.factor(ctrl2),
               alpha=as.factor(ctrlinten)),
           stat = "identity",position = "dodge")+
  #geom_hline(yintercept = 0.1,color="red",size=0.5)+
  scale_alpha_manual(breaks =c(30,40,50,60,70,80),
                     values=c(0.3,0.4,0.5,0.6,0.7,0.8))+
  geom_errorbar( aes(x=as.factor(ctrlinten), ymin=low, 
                     ymax=upper ,group= as.factor(ctrl)),
                 position = position_dodge(width=0.5),
                 width=0.5, alpha=1, size=0.5,color="black")+
  scale_fill_manual(values = mypal[4])+
  theme_classic()+
  scale_x_discrete(breaks = unique(dfag$ctrlinten),labels = paste(unique(dfag$ctrlinten),"%",sep=""))+
  scale_y_continuous(expand = c(0,0),
                     breaks = c(0,0.025,0.05,0.075),
                     labels = c("90%","92.5%","95%","97.5%"),
                     limits = c(0,0.08)
  )+
  coord_flip(clip="off")+
  theme(
    panel.background = element_rect(fill = "transparent"), # bg of the panel
    plot.background = element_rect(fill = "transparent"), # bg of the plot
    legend.position = 'none',
    legend.title = element_blank(),
    strip.background = element_blank(),
    axis.ticks = element_line(colour = "black",size = 0.5),
    axis.ticks.length=unit(.25, "cm"),
    axis.text.x = element_text(margin = margin(t =5, r = 0, b = 0, l = 0)),
    axis.text.y = element_text(margin = margin(t = 0, r =5, b = 0, l = 0)),
    panel.spacing = unit(2, "lines"),
    text =element_text(size =30),
    #plot.margin = margin(0.1, 0.1, 0.1, 0.1, "cm"),
    axis.title = element_blank(),
    aspect.ratio = 1)
t


ggsave("./figs/fig3d.jpeg", t,  width = 23, height = 23, units = "cm",dpi=1200,bg="transparent")

ggsave("./figs/fig3d.svg", t,  width = 23, height = 23, units = "cm",dpi=1200,bg="transparent")
