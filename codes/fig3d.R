library(readr)
library(ggplot2)
library(dplyr)
library(RColorBrewer)
library(ggsci)
library(ggthemes)
library(lemon)
library(mratios)
####################
filenames<-list.files("../data/outagebasebaseline/","df[0-9]+.csv",full.names = TRUE,recursive = TRUE)

df<-c()

i<-1
while (i<= length(filenames)){
  dfsp <- data.frame(read_csv (filenames[i]))
  df<-rbind(df,dfsp)
  i<-i+1
}
####################

df1<-subset(df,ssl=="low" & r0==2.4 & start == "Wuhan_NA_China")

df365<-data.frame(subset(df1,days<=365) %>%
                    group_by(itr) %>%
                    filter(days == max(days)))


df365$ci<-df365$ci/df365$n

df2<- df365 %>%
  group_by(region,age) %>%
  summarise(mean.mpg = mean(ci, na.rm = TRUE),
            sd.mpg = sd(ci, na.rm = TRUE),
            n.mpg = n()) %>%
  mutate(se.mpg = sd.mpg / sqrt(n.mpg),
         lower.ci.mpg = mean.mpg- qt(1 - (0.05 / 2), n.mpg - 1) * se.mpg,
         upper.ci.mpg = mean.mpg + qt(1 - (0.05 / 2), n.mpg - 1) * se.mpg)

ggplot(data=df2)+
  geom_line(aes(x=age,y=mean.mpg,group=1))+
  geom_ribbon(aes(x=age,ymin=lower.ci.mpg,ymax=upper.ci.mpg))+
  scale_fill_viridis_d()+
  theme_classic()+
  facet_wrap(~region)
######################################################

df365<-data.frame(subset(df1,days<=365) %>%
                    group_by(itr) %>%
                    filter(days == max(days)))

df365ag<-aggregate(df365[c("n","ci")],by=df365[c("itr","age")],FUN="sum")


df365ag$ci<-df365ag$ci/df365ag$n

df22<- df365ag %>%
  group_by(age) %>%
  summarise(mean.mpg = mean(ci, na.rm = TRUE),
            sd.mpg = sd(ci, na.rm = TRUE),
            n.mpg = n()) %>%
  mutate(se.mpg = sd.mpg / sqrt(n.mpg),
         lower.ci.mpg = mean.mpg- qt(1 - (0.05 / 2), n.mpg - 1) * se.mpg,
         upper.ci.mpg = mean.mpg + qt(1 - (0.05 / 2), n.mpg - 1) * se.mpg)


page<-ggplot(data=df22)+
  geom_bar(aes(x=as.factor(age),y=mean.mpg,group=1,fill=as.factor(age)),stat = "identity",position = "stack")+
  scale_fill_viridis_d()+
  theme_classic()+
  scale_x_discrete(expand=c(0,0),
    breaks = c(0,5,10,15),labels=c("0-5","25-30","50-55","75-"))+
  scale_y_continuous(expand = c(0,0),
                     breaks = c(0,0.25,0.5,0.75),
                     labels = c("0%","25%","50%","75%"),
                     limits = c(0,0.95))+
  #coord_flip(clip="off")+
  
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
    #panel.spacing = unit(2, "lines"),
    text =element_text(size =20),
    axis.title = element_blank(),
    aspect.ratio = 1)
page


ggsave("../figs/fig3d.jpeg", page,  width =10, height = 10, units = "cm",dpi=1200,bg="transparent")


ggsave("../figs/fig3d.svg", page,  width =10, height = 10, units = "cm",dpi=1200,bg="transparent")
