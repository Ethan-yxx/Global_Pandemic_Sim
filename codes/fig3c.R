library(readr)
library(ggplot2)
library(dplyr)
library(RColorBrewer)
library(ggsci)
library(ggthemes)
library(lemon)
library(reshape2)
library(reshape)
library(data.table)
###########################
df<-data.frame(read_csv("../data/basenew.csv"))
###########################
#compare the first year cumulative infections
df<-subset(df, !is.na(ci))
names(df)[which(names(df)=="start")]<-"nameu"
df<-subset(df, ssl=="low" & r0 == 2.4 & 
             nameu=="Wuhan_NA_China")

######################################################################################################################
dfi<-data.frame(df[c("n","ic","ia","r0","itr","days")] %>%
                  group_by(r0,itr,days) %>%
                  summarise(n=sum(n),
                            ic=sum(ic),
                            ia=sum(ia)))

dfi$i<-(dfi$ic+dfi$ia)/dfi$n
dfi<-dfi[-which(names(dfi) %in% c("n","ia","ic"))]

dfc<-data.frame(df[c("n","ci","r0","itr","days")] %>%
                  group_by(r0,itr,days) %>%
                  summarise(n=sum(n),
                            ci=sum(ci)))
dfc$ci<-dfc$ci/dfc$n
dfc<-dfc[-which(names(dfc) %in% c("n","ia","ic"))]
######################################################################################################################






######################################################################################################################

dfi2<-melt(dfi,id =c("r0","itr","days"))

dfc2<-melt(dfc,id =c("r0","itr","days"))

dfcm<-rbind(dfi2,dfc2)

dfi3<- subset(dfcm) %>%
  group_by(days,variable) %>%
  summarise(mean.mpg = mean(value, na.rm = TRUE),
            sd.mpg = sd(value, na.rm = TRUE),
            n.mpg = n()) %>%
  mutate(se.mpg = sd.mpg / sqrt(n.mpg),
         lower.ci.mpg = mean.mpg- 1.96 * se.mpg,
         upper.ci.mpg = mean.mpg + 1.96 * se.mpg)



coef1<-max(subset(dfi3,variable=="ci")$mean.mpg)/
  max(subset(dfi3,variable=="i")$mean.mpg,na.rm = TRUE)

coef2<-max(subset(dfi3,variable=="ci")$lower.ci.mpg,
           na.rm = TRUE )/
  max(subset(dfi3,variable=="i")$mean.mpg, na.rm = TRUE )

coef3<-max(subset(dfi3,variable=="ci")$upper.ci.mpg ,
           na.rm = TRUE )/
  max(subset(dfi3,variable=="i")$mean.mpg,na.rm = TRUE )

dfi3[which(dfi3$variable=="ci"),c(3,7,8)]<-
  sweep(dfi3[which(dfi3$variable=="ci"),c(3,7,8)],2,
        c(coef1,coef1,coef1),"/")

#############################################################################
cls<-pal_npg("nrc")(9)
library("scales")
show_col(pal_npg("nrc")(9))

icplot<-ggplot(data= subset(dfi3,days<=366))+
  geom_line(aes(x=days-1,y = mean.mpg,
                linetype=as.factor(variable)),color=cls[9])+
  geom_ribbon(aes(x=days,ymin= lower.ci.mpg,ymax=upper.ci.mpg,
                  group=as.factor(variable)),fill=cls[9],alpha=0.3)+
  scale_color_npg()+
  scale_fill_npg()+
  scale_x_continuous(expand = c(0,0),
                     breaks = c(0,200,400,600)/2,
                     labels =c(0,200,400,600)/2)+
  scale_y_continuous(expand = c(0,0),
                     breaks = c(0,0.01,0.02,0.03),
                     labels = c("0%","1%","2%","3%"),
                     limits = c(0,0.031),sec.axis = sec_axis
                     ( trans=~.*coef1,labels=scales::percent))+
  theme_classic()+
  #facet_rep_wrap(~ssl, repeat.tick.labels = TRUE)+
  #coord_capped_cart(bottom='both', left='both')+
  theme(
    panel.background = element_rect(fill = "transparent"), # bg of the panel
    plot.background = element_rect(fill = "transparent"), # bg of the plot
    legend.position = 'none',
    legend.title = element_blank(),
    strip.background = element_blank(),
    #strip.text = element_blank(),
    axis.ticks = element_line(colour = "black",size = 0.5),
    #axis.ticks.length=unit(.25, "cm"),
    axis.text.x = element_text(margin = margin(t =5, r = 0, b = 0, l = 0)),
    axis.text.y = element_text(margin = margin(t = 0, r =5, b = 0, l = 0)),
    #plot.margin = margin(1, 1, 1, 1, "cm"),
    #panel.spacing = unit(2, "lines"),
    text =element_text(size =20),
    axis.title = element_blank(),
    aspect.ratio = 1)

icplot

# ggsave("../figs/fig3c.jpeg", icplot,  width = 10, height = 10, units = "cm",dpi=1200,bg="transparent")
# ggsave("../figs/fig3c.svg", icplot,  width = 10, height = 10, units = "cm",dpi=1200,bg="transparent")

