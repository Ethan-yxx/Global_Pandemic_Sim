rm(list=ls())
library(readr)
library(ggplot2)
library(dplyr)
library(RColorBrewer)
library(ggsci)
library(ggthemes)
library(lemon)
library(mratios)
library(egg)
###########################
df<-data.frame(read_csv("./data/dfspwh_nn.csv"))
###
df<-subset(df,!is.na(ci) & r0==2.4)

df<- df[c("n","ci","days","ctrl","itr")] %>%
  group_by(days,ctrl,itr) %>%
  summarise(n = sum(n, na.rm = TRUE),
            ci = sum(ci, na.rm = TRUE)) 


df365<-data.frame(subset(df,days<=365)[c("n","ci","ctrl","itr","days")] %>%
                    group_by(ctrl,itr) %>%
                    filter(days == max(days)))
min(df365$days)
###
dfbase<-data.frame(read_csv("./data/basenew.csv"))

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
dfag$ctrl2 <- factor(dfag$ctrl2, levels = c("schoolclose","workclose","otherclose","travelrestrict"))
dfag$ctrlinten<-as.numeric(as.character(gsub("[a-z]+|_","",dfag$ctrl)))
#########

t<-ggplot(data= subset(dfag,ctrlinten %in% c(10,30,50,70,90,95,99,99.9)))  +
  
  geom_bar(aes(x=as.factor(ctrlinten),y = ci_mean ,fill= as.factor(ctrl2)),stat = "identity",position = "dodge")+
  #geom_hline(yintercept = 0.1,color="red",size=0.5)+
  geom_errorbar( aes(x=as.factor(ctrlinten), ymin=low, ymax=upper ,group= as.factor(ctrl2)),
                 position = position_dodge(width=0.9),
                 width=0.5, alpha=1, size=0.5,color="black")+
  scale_fill_npg()+
  theme_classic()+
  scale_x_discrete(breaks = unique(dfag$ctrlinten),labels = paste(unique(dfag$ctrlinten),"%",sep=""))+
  scale_y_continuous(expand = c(0,0),
                     breaks = c(0,0.25,0.5,0.75,1),
                     labels = c("0%","25%","50%","75%","100%"),
                     limits = c(0,1.1)
  )+
  coord_flip(clip="off")+
  theme(
    panel.background = element_rect(fill = "transparent"), # bg of the panel
    plot.background = element_rect(fill = "transparent"), # bg of the plot
    legend.position = 'none',
    legend.title = element_blank(),
    strip.background = element_blank(),
    axis.ticks = element_line(colour = "black",size = 0.5),
    #axis.ticks.length=unit(.25, "cm"),
    axis.text.x = element_text(margin = margin(t =5, r = 0, b = 0, l = 0)),
    axis.text.y = element_text(margin = margin(t = 0, r =5, b = 0, l = 0)),
    panel.spacing = unit(2, "lines"),
    text =element_text(size =30),
    #plot.margin = margin(0.1, 0.1, 0.1, 0.1, "cm"),
    axis.title = element_blank(),
    #aspect.ratio = 1
    )
t

tpfix <- set_panel_size(t,width  = unit(20, "cm"),
                        height = unit(20, "cm"))
ggsave("../figs/fig3a.jpeg", tpfix,  width =25,
       height = 25, units = "cm",dpi=1200,bg="transparent")
ggsave("../figs/fig3a.svg", tpfix,  width =25, 
    height = 25, units = "cm",dpi=1200,bg="transparent")


write.csv(dfag,"../stats/dfspcum.csv",row.names = FALSE)


