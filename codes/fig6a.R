library(ggplot2)
library(ggrepel)
library(ggsci)
library(RColorBrewer)
library(wesanderson)
library(readr)
library(egg)
###########################################################
dfcompare<-readRDS("./data/dfcompareallwh_01_per2.RDS")
dfcompare$incomegroup[is.na(dfcompare$incomegroup)]<-"High income"
tcount<-readRDS("./data/tcoundwh.RDS")
s1<-data.frame(read_csv("./data/s1.csv"))
###########################################################
dfc<-merge(dfcompare,tcount,
           by="maskcountry",all.x=TRUE)
nrow(subset(dfcompare,sigm==1))

dfc$cc<-dfc$ci.y-dfc$ci.x

testjp<-subset(dfc,maskcountry=="JPN")
ttestratio(subset(dfci,ctrl==uctrl[x])$ci,
           subset(dfci,ctrl=="baseline")$ci )

testjp$n.y*0.001780032
testjp$n.y*0.001491355
testjp$n.y*0.001639691/testjp$ci.x
###########################################################
dfc<-merge(dfc,s1[c("maskcountry","sp")],
           by="maskcountry",all.x=TRUE)
isoregion<-data.frame(read.csv("./data/gstout2.csv"))

dfc<-merge(dfc,isoregion,by.x = "maskcountry",
           by.y="CTR_MN_ISO",all.x = TRUE)

ggplot()+
  geom_text(data=subset(dfc,maskcountry !="CHN")
             ,aes(x=n,y=per2,
                  color=as.factor(incomegroup),label=maskcountry))
wes_palette("Chevalier1", 4, type = c("discrete"))

unique(subset(dfc,sigm==1 & maskcountry !="CHN")$incomegroup)
t<-ggplot()+
  geom_point(data=subset(dfc,sigm==1 & maskcountry !="CHN"),
            aes(x=n,y =pc,
                color=as.factor(GRGN_L1),size=gdp),
           alpha=0.7)+
  geom_point(data=subset(dfc,sigm==0 & maskcountry !="CHN"),
           aes(x=n,y =pc,
               size=gdp),color="grey",alpha=0.7)+
  geom_text_repel(data=subset(dfc,sigm==1 & maskcountry !="CHN"),
                  aes(x=n,y =pc,label=maskcountry),size=15*5/14)+
  scale_color_brewer(palette = "Dark2")+
  theme_classic()+
  scale_size(range = c(0.5, 25),guide = FALSE)+
  scale_y_continuous(breaks=c(0,0.0005,0.001,0.0015),
                     labels = c("0%","0.05%","0.10%","0.15%"),
                     limits = c(-0.0001,0.0019),
                     expand = c(0,0))+
  scale_x_continuous(expand=c(0,0),
                     breaks=c(0,30,60,90,120,150),
                     limits = c(0,155))+
  guides(
    color= guide_legend(override.aes = list(size=5))
  )+
  theme(
    panel.background = element_rect(fill = "transparent"), # bg of the panel
    plot.background = element_rect(fill = "transparent"), # bg of the plot
    legend.position = c(0.25,0.90),
    legend.key.size = unit(2, 'lines'),
    legend.title = element_blank(),
    strip.background = element_blank(),
    axis.ticks = element_line(colour = "black",size = 0.5),
    #axis.ticks.length=unit(.25, "cm"),
    axis.text.x = element_text(margin = margin(t =5, r = 0, b = 0, l = 0)),
    axis.text.y = element_text(margin = margin(t = 0, r =5, b = 0, l = 0)),
    #panel.spacing = unit(2, "lines"),
    text =element_text(size =30),
    axis.title = element_blank(),
    aspect.ratio = 1)
t


tpfix <- set_panel_size(t,width  = unit(20, "cm"),
                        height = unit(20, "cm"))
ggsave("../figs/fig4a.jpeg", tpfix,  width =25,
       height = 25, units = "cm",dpi=1200,bg="transparent")
ggsave("../figs/fig4a.svg", tpfix,  width =25, 
       height = 25, units = "cm",dpi=1200,bg="transparent")
