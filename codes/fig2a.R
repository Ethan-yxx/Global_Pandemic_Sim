library(readr)
library(readr)
library(ggplot2)
library(dplyr)
library(RColorBrewer)
library(ggsci)
library(ggthemes)
library(lemon)
library(reshape2)
library(tibble)
library(egg)
###########################
df<-data.frame(read_csv("../data/base300random.csv"))
###########################
df<-subset(df, !is.na(ci))
#compare the first year cumulative infections
names(df)[which(grepl('13',names(df)))]<-"nameu"
odarrayr<-readRDS("../data/odarrayr3157.rds")
###########################
ratio<-c(rep(c(31,30),3),31,31,rep(c(30,31),2))
od1<-sweep(odarrayr,3,ratio,"*")
n1<-apply(od1,1,FUN="sum")
q30<-data.frame(read_csv("../data/qtall.csv"))
q30<-q30[order(q30$index),]
q<-data.frame(nameu=q30$nameu,osum=n1*2)
df22<-subset(df,days<=365)
###########################
df22<-merge(df22[c("n","ci","r0","itr","days","nameu")],
            q,by="nameu",all.x=TRUE)
dfc<-data.frame(df22[c("n","ci","r0","itr","days","nameu","osum")] %>%
                  group_by(r0,itr,days,nameu,osum) %>%
                  summarise(n=sum(n),
                            ci=sum(ci)))
dfc$ci<-dfc$ci/dfc$n
dfc<-dfc[-6]
# dfc$level[which(dfc$nameu=="Wuhan_NA_China")]<-"z"
df3<-dfc %>%
  group_by(nameu,days,osum) %>%
  dplyr::summarise(meanci=mean(ci),
                   x=list(enframe(quantile(
                     ci, probs=c(0.25,0.75),na.rm=TRUE), 
                     "quantiles", "value"))) %>% 
  tidyr::unnest(x)

df3$quantiles<-paste("p",gsub("%","",df3$quantiles),sep="")

dag<-reshape::cast(df3,nameu+days+osum+meanci~quantiles)

#saveRDS(dag,"dag.RDS")
#dag$level[which(dag$nameu=="Wuhan_NA_China")]<-"z"
# cls<- pal_npg("nrc")(3)


t<-ggplot()+
  geom_line(data=subset(dag,!is.na(osum) 
                        & nameu!="Wuhan_NA_China")
            ,aes(x=days-1,y=meanci,
                 color=log10(osum),group=nameu),
            alpha=0.8,size=0.01)+
  geom_ribbon(data=subset(dag,!is.na(osum)
                          & nameu!="Wuhan_NA_China"),
              aes(x= days,ymin=p25,
                  ymax=p75,
                  fill=log10(osum),group=nameu),alpha=0.1)+
 
  # geom_ribbon(data=subset(dag,nameu == "Wuhan_NA_China"),
  #             aes(x= days,ymin=lower.ci.mpg,
  #                 ymax=upper.ci.mpg),fill="black",alpha=0.7)+
  geom_line(data=subset(dag,nameu == "Wuhan_NA_China")
            ,aes(x=days-1,y=meanci ),color="red",size=1.5)+
  scale_color_viridis_c()+
  scale_fill_viridis_c()+
  theme_classic()+
  scale_y_continuous(expand = c(0,0),
                     breaks = c(0,0.25,0.5,0.75),
                     labels = c("0%","25%","50%","75%"),
                     limits=c(0,0.8))+
  scale_x_continuous(expand=c(0,0),
                     breaks=c(0,100,200,300))+
  guides(fill = guide_colourbar(barwidth=2,
                                barheight = 15),
        color = guide_colourbar(barwidth=2,
                                barheight = 15))+
  theme(
    panel.background = element_rect(fill = "transparent"), # bg of the panel
    plot.background = element_rect(fill = "transparent"), # bg of the plot
    legend.position = c(0.1,0.8),
    legend.title = element_blank(),
    strip.background = element_blank(),
    axis.ticks = element_line(colour = "black",size = 0.5),
    # axis.ticks.length=unit(.25, "cm"),
    axis.text.x = element_text(margin = margin(t =5, r = 0, b = 0, l = 0)),
    axis.text.y = element_text(margin = margin(t = 0, r =5, b = 0, l = 0)),
    #panel.spacing = unit(2, "lines"),
    text =element_text(size =30),
    axis.title = element_blank()
    )
t

tpfix <- set_panel_size(t,width  = unit(20, "cm"),
                        height = unit(20, "cm"))
# ggsave("../figs/fig2a.jpeg", tpfix,  width =25,
#        height = 25, units = "cm",dpi=1200,bg="transparent")
# ggsave("../figs/fig2a.svg", tpfix,  width =25, 
#        height = 25, units = "cm",dpi=1200,bg="transparent")

