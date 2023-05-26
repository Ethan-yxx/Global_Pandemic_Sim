library(readr)
library(ggplot2)
library(dplyr)
library(RColorBrewer)
library(ggsci)
library(ggthemes)
library(lemon)
library(reshape2)
library(classInt)
library(tibble)
library(egg)
###########################
df<-rbind(data.frame(read_csv("./data/df365n.csv")),
          data.frame(read_csv("./data/df300_365.csv")))

iatatbr<-readRDS("./data/iatatb3157.rds")

s1<-data.frame(nameu=iatatbr$nameu,
               pop=iatatbr$UN_2020_E)
ratio<-c(rep(c(31,30),3),31,31,rep(c(30,31),2))

n1<-readRDS("./data/n41.rds")
q30<-data.frame(read_csv("./data/qtall.csv"))
q30<-q30[order(q30$index),]

q<-data.frame(nameu=q30$nameu,osum=n1*2)
sum(q$osum>=10^4.5)/nrow(q)
###########################
df<-subset(df, !is.na(ci))
#compare the first year cumulative infections
names(df)[which(names(df)=="start")]<-"nameu"
df1<-data.frame(df[c("n","ci","nameu","itr","days")] %>%
                  group_by(nameu,itr,days) %>%
                  summarise(n=sum(n),
                            ci=sum(ci)))
test<-subset(df,nameu=="Shanghai_NA_China")
dfbase365<-data.frame(subset(df1,days<=365) %>%
                        group_by(nameu,itr) %>%
                        filter(days == max(days)))
# dfbase365<-subset(dfbase365,ci !=0)
length(unique(dfbase365$nameu))
dfbase365$ci<-dfbase365$ci/dfbase365$n

df2<-merge(subset(df1,days<=365),q,by="nameu",all.x=TRUE)

nrow(subset(df2,osum>4.5))/3057

dfc<-dfbase365 %>%
  group_by(nameu) %>%
  summarise(ci = mean(ci),
            num=n())

dfbase365<-dfbase365 %>%
  group_by(nameu) %>%
  summarise(ci = mean(ci))


df2<-merge(dfbase365,q,by="nameu",all.x=TRUE)

df2$osum<-log10(df2$osum)

df2<-subset(df2,!is.na(osum))
df2<-merge(df2,s1,by="nameu",all.x=TRUE)

df2<-df2[order(df2$osum),]


h<-classIntervals(df2$osum,
                  style = "equal",n=50)
h1<-findCols(h)

df2$rank<-h1

df3<-df2 %>%
  group_by(rank) %>%
  dplyr::summarise(osum=mean(osum),
                   meanci=mean(ci),
                   x=list(enframe(quantile(
                     ci, probs=c(0.25,0.75),na.rm=TRUE), 
                     "quantiles", "value"))) %>% 
  tidyr::unnest(x)

df3$quantiles<-paste("p",gsub("%","",df3$quantiles),sep="")

df4<-reshape::cast(df3,rank+osum+meanci~quantiles)

g1<-gam(data=subset(df4),meanci~s(osum,bs="cr"))
summary(g1)
plot(g1)

###########################
t<-ggplot(data = subset(df4))+
  geom_point(aes(x=osum,y=meanci),color="blue",size=2.5)+
  #geom_ribbon(aes(x=osum,ymin=p25,ymax=p75),alpha=0.2)+
  geom_smooth(method="gam",formula = y~s(x,bs="cr"),
              aes(x=osum,y=meanci),color="red",size=2)+
  theme_classic()+
  scale_x_continuous(expand = c(0,0))+
  scale_y_continuous(expand = c(0,0),breaks=c(0.25,0.5,0.75),
                     labels=c("25%","50%","75%"))+
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
    # plot.margin = margin(0.1, 0.1, 0.1, 0.1, "cm"),
    axis.title = element_blank())

t

tpfix <- set_panel_size(t,width  = unit(20, "cm"),
                        height = unit(20, "cm"))

ggsave("../figs/fig2b.jpeg", tpfix,  width =25,
       height = 25, units = "cm",dpi=1200,bg="transparent")
ggsave("..figs/fig2b.svg", tpfix,  width =25, 
       height = 25, units = "cm",dpi=1200,bg="transparent")











