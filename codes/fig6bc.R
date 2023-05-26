library(rgdal)
library(rsatscan)
library(igraph)
library(ggrepel)
library(writexl)
library(scales)
library(ISLR)
library(mgcv)
library(voxel)
library(tidyverse)
library(gridExtra)
iatatbr<-readRDS("./data/iatatb3157.rds")
n1<-readRDS("./data/n61.rds")
osum<-apply(n1,1,sum)
odmp<-sweep(n1,1,osum,"/")
odmp<-as.matrix(1-log(odmp))
odmp[is.infinite(odmp)]<-0
g <- graph.adjacency(odmp, weighted=TRUE)
s.paths <- shortest.paths(g, algorithm = "dijkstra",mode="out")
#####################################################################
whid<-which(iatatbr$nameu=="Wuhan_NA_China")
w1<-s.paths[whid,]
w2<-s.paths[,whid]
s1<-data.frame(nameu=iatatbr$nameu,sp=w1,maskcountry=iatatbr$ISOALPHA ,pop=iatatbr$UN_2020_E)
q30<-data.frame(read_csv("qtall.csv"))
s1<-merge(s1,q30,by="nameu",all.x=TRUE)
###########################################################
dfcompare<-readRDS("./data/dfcompareallwh_01_per2.RDS")
dfcompare$incomegroup[is.na(dfcompare$incomegroup)]<-"High income"
tcount<-readRDS("./data/tcoundwh.RDS")
dfc<-merge(dfcompare,tcount,
           by="maskcountry",all.x=TRUE)
nrow(subset(dfcompare,sigm==1))
dfc$cc<-dfc$ci.y-dfc$ci.x
#####################################################################
df2<-merge(s1,dfc,by="maskcountry",all.x=TRUE)
df2<-subset(df2,!is.na(osum) & !is.na(gdpper) & !is.infinite(sp) & 
              maskcountry !="CHN" 
            #& sigm==1
            )
#df2<-subset(df2, !is.infinite(sp))
df3<-df2 %>%
  group_by(maskcountry) %>%
  summarise(pc=mean(pc,na.rm = TRUE),
            sp=mean(sp,na.rm = TRUE),
            gdp=mean(gdp,na.rm = TRUE),
            gdpper=mean(gdpper,na.rm = TRUE),
            osum=sum(osum,na.rm = TRUE),
            pop=mean(pop,na.rm = TRUE))
# write.csv(df3,"s1.csv")
df3$gdp<-log10(df3$gdp)
g1<-gam(data=subset(df3),pc~s(sp,bs="cr")+
          s(gdp,bs="cr"))
summary(g1)
plot(g1)
#####################################################################
t1<-plotGAM(g1, smooth.cov = "gdp")+
  scale_color_manual(values = c("red"))+
  theme_classic()+
  scale_x_continuous(expand = c(0,0))+
  scale_y_continuous(expand = c(0,0),labels=percent_format(accuracy = 0.01))+
  labs(title = "")+
  theme(
    panel.background = element_rect(fill = "transparent"), # bg of the panel
    plot.background = element_rect(fill = "transparent"), # bg of the plot
    legend.position = 'none',
    legend.title = element_blank(),
    strip.background = element_blank(),
    axis.ticks = element_line(colour = "black",size = 0.5),
    # axis.ticks.length=unit(.25, "cm"),
    axis.text.x = element_text(margin = margin(t =5, r = 0, b = 0, l = 0)),
    axis.text.y = element_text(margin = margin(t = 0, r =5, b = 0, l = 0)),
    panel.spacing = unit(2, "lines"),
    text =element_text(size =30),
    axis.title = element_blank())
t1

tpfixb <- set_panel_size(t1,width  = unit(10, "cm"),
                        height = unit(8, "cm"))
ggsave("../figs/fig4b.jpeg", tpfixb,  width =15,
       height = 10, units = "cm",dpi=1200,bg="transparent")
ggsave("../figs/fi4b.svg", tpfixb,  width =15, 
       height = 10, units = "cm",dpi=1200,bg="transparent")
#####################################################################
t2<-plotGAM(g1, smooth.cov = "sp")+
  scale_color_manual(values = c("red"))+
  theme_classic()+
  scale_x_continuous(expand = c(0,0))+
  scale_y_continuous(expand = c(0,0),labels=percent_format(accuracy = 0.01))+
  labs(title = "")+
  theme(
    panel.background = element_rect(fill = "transparent"), # bg of the panel
    plot.background = element_rect(fill = "transparent"), # bg of the plot
    legend.position = 'none',
    legend.title = element_blank(),
    strip.background = element_blank(),
    axis.ticks = element_line(colour = "black",size = 0.5),
    # axis.ticks.length=unit(.25, "cm"),
    axis.text.x = element_text(margin = margin(t =5, r = 0, b = 0, l = 0)),
    axis.text.y = element_text(margin = margin(t = 0, r =5, b = 0, l = 0)),
    panel.spacing = unit(2, "lines"),
    text =element_text(size =30),
    axis.title = element_blank())

tpfixc <- set_panel_size(t2,width  = unit(10, "cm"),
                         height = unit(8, "cm"))
ggsave("../figs/fig4c.jpeg", tpfixc,  width =15,
       height = 10, units = "cm",dpi=1200,bg="transparent")
ggsave("../figs/fig4c.svg", tpfixc,  width =15, 
       height = 10, units = "cm",dpi=1200,bg="transparent")

