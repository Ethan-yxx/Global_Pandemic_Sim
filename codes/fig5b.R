library(ggplot2)
library(reshape)
library(readxl)
library(dplyr)
library(ggsci)
library(lemon)
library(readr)
library(scales)
library(egg)
library(tibble)
###############################################################
cmr<-readRDS("../data/iatacm3157new.RDS")
cm1<-data.frame(t(sapply(1:length(cmr),function(x){return(c(
  sum(cmr[[x]]$school),
  sum(cmr[[x]]$work),
  sum(cmr[[x]]$other),
  sum(cmr[[x]]$home)
))})))
names(cm1)<-c("school","work","other","home")
cm2<-sweep(cm1,1,apply(cm1,1,sum),"/")

###############################################################
iatatb<-readRDS("../data/iatatb3157.RDS")
isoregion<-data.frame(read.csv("../data/gstout2.csv"))

cm2$CTR_MN_ISO<-iatatb$ISOALPHA

cm2<-merge(cm2,isoregion,by="CTR_MN_ISO",all.x = TRUE)
cm2$id<-1:nrow(cm2)

cm3<-melt(cm2[c(2:6,8)],id=c("id","GRGN_L1"))

ggplot(data=cm2)+
  geom_violin(aes(x=GRGN_L1,y=school))


# cm4<-data.frame(cm3 %>%
#                   group_by(GRGN_L1,variable) %>%
#                   summarise(mean.mpg = mean(value, na.rm = TRUE),
#                             sd.mpg = sd(value, na.rm = TRUE),
#                             n.mpg = n()) %>%
#                   mutate(se.mpg = sd.mpg / sqrt(n.mpg),
#                          lower.ci.mpg = mean.mpg- qt(1 - (0.05 / 2), n.mpg - 1) * se.mpg,
#                          upper.ci.mpg = mean.mpg + qt(1 - (0.05 / 2), n.mpg - 1) * se.mpg))
cm4<-cm3 %>%
  group_by(GRGN_L1,variable) %>%
  dplyr::summarise(meanvalue=mean(value,na.rm=TRUE),
                   x=list(enframe(quantile(
                     value, probs=c(0.05,0.95),na.rm=TRUE), 
                     "quantiles", "value"))) %>% 
  tidyr::unnest(x)
cm4$quantiles<-paste("p",gsub("%","",cm4$quantiles),sep="")

cm4<-reshape::cast(cm4,GRGN_L1+variable+
                     meanvalue~quantiles)


cm4$GRGN_L1<-as.character(cm4$GRGN_L1)
cm4$GRGN_L1[grepl("Latin", cm4$GRGN_L1)]<-"Latin America"
#################################
cls<-pal_npg(palette = c("nrc"),alpha=1)(3)
cm4$variable<-factor(cm4$variable,
          levels=rev(c("school","work","other")))

t<-ggplot(data=subset(cm4,variable !="home"))+
 
  geom_errorbar(aes(x=GRGN_L1,ymin=p5,
                    ymax=p95,color=variable),
                width=0,size=2)+
  geom_point(aes(x=GRGN_L1,y=meanvalue,color=variable),size=5)+
  scale_y_continuous(label=percent_format(accuracy = 1))+
  facet_rep_grid(variable~.,scales="free")+
  scale_color_manual(values=rev(cls))+
  theme_classic()+
  theme(
  panel.background = element_rect(fill = "transparent"), # bg of the panel
  plot.background = element_rect(fill = "transparent"), # bg of the plot
  legend.position = "none",
  legend.title = element_blank(),
  strip.background = element_blank(),
  strip.text = element_blank(),
  axis.ticks = element_line(colour = "black",size = 0.5),
  # axis.ticks.length=unit(.25, "cm"),
  axis.text.x=element_blank(),
  #axis.text.x = element_text(margin = margin(t =5, r = 0, b = 0, l = 0)),
  axis.text.y = element_text(margin = margin(t = 0, r =5, b = 0, l = 0)),
  panel.spacing = unit(1.5, "lines"),
  text =element_text(size =30),
  axis.title = element_blank())
t

tpfix <- set_panel_size(t,width  = unit(20, "cm"),
                        height = unit(6, "cm"))

ggsave("../figs/fig5b.jpeg", tpfix,  width =25,
       height = 25, units = "cm",dpi=1200,bg="transparent")
ggsave("./figs/fig5b.svg", tpfix,  width =25, 
       height = 25, units = "cm",dpi=1200,bg="transparent")




