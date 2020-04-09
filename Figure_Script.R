rm(list=ls(all=TRUE)) 

library(readxl);library(tidyverse);library(janitor);library(cowplot);library(plotrix)
####################### FORMATTING GROWTH  ########################################################### #####
setwd("./")
library(readxl);library(tidyverse);library(janitor);library(cowplot)

growth_AH<-read_xlsx("Data/Growth_03062020.xlsx")%>%clean_names()%>%
     mutate(type=case_when(str_detect(aggregate_individual, "[A-O]") ~"agg",
                           TRUE ~ "ind"))%>%
     rename(growth=growth_mm2)%>%
     rename(change=growth)%>%
     mutate(growth=(change/initial_july)*100)

saveRDS(growth_AH,"Data/growthdata")
####################### FORMATTING SURVIVORSHIP ###################################################### #####
setwd("./")
library(readxl);library(tidyverse);library(janitor)

raw<-read_xlsx("Data/M.cap_Survivorship_03032020.xlsx")%>%clean_names()
metadata<-raw[,1:5]

initial<-raw%>%select(plug_number,temperature,conditioned,flow,growth_shade,noquote(order(colnames(raw))))%>%
     select(-contains("part"))%>%
     select(-contains("x"))%>%
     gather(measurement,count,-plug_number,-temperature,-conditioned,-flow,-growth_shade)%>%
     separate(measurement,into=c("measurement","date"),sep=-6)%>%
     mutate(measurement=case_when(measurement=="agg_"~"agg_surv",
                                  measurement=="ind_"~"ind_surv",
                                  measurement=="agg_bleach_"~"agg_bleach",
                                  measurement=="ind_bleach_"~"ind_bleach"))%>%
     separate(measurement,into=c("type","measurement"),sep="_")%>%
     filter(date=="071519"|date=="071719")%>%
     select(-temperature,-conditioned,-flow,-growth_shade,-date)%>%
     mutate(count=as.numeric(count))%>%
     group_by(plug_number,type)%>%summarise(count=sum(count,na.rm=TRUE))%>%
     rename(initial=count)

counts<-raw%>%select(plug_number,temperature,conditioned,flow,growth_shade,noquote(order(colnames(raw))))%>%
     select(-contains("part"))%>%
     select(-contains("x"))%>%
     gather(measurement,count,-plug_number,-temperature,-conditioned,-flow,-growth_shade)%>%
     separate(measurement,into=c("measurement","date"),sep=-6)%>%
     separate(measurement,into=c("type","measurement"))%>%mutate(measurement=case_when(measurement!="bleach" ~ "surv",
                                                                                       TRUE ~ as.character(measurement)))%>%
     mutate(timepoint=case_when(date=="071519"~1,
                                date=="071719"~1,
                                date=="071819"~1,
                                date=="072219"~2,
                                date=="072419"~2,
                                date=="072919"~3,
                                date=="073119"~3,
                                date=="080519"~4,
                                date=="080719"~4,
                                date=="081219"~5,
                                date=="081419"~5,
                                date=="081919"~6,
                                date=="082119"~6,
                                date=="091719"~7,
                                date=="091919"~7,
                                date=="101519"~8,
                                date=="101719"~8,
                                date=="111219"~9,
                                date=="111419"~9,
                                date=="121019"~10,
                                date=="121219"~10))%>%
     select(-temperature,-conditioned,-flow,-growth_shade,-date)%>%
     mutate(count=as.numeric(count))%>%
     group_by(plug_number,timepoint,type)%>%summarise(count=sum(count,na.rm=TRUE))

surv<-left_join(left_join(counts,initial,by=c("plug_number","type"))%>%
     select(plug_number,timepoint,type,initial,count)%>%
     mutate(dead=as.numeric(initial)-as.numeric(count))%>%
     mutate(dead=case_when(dead<0 ~ 0,TRUE ~ as.numeric(dead)))%>%
     select(-initial),metadata,by="plug_number")%>%
     select(plug_number,temperature,conditioned,flow,growth_shade,timepoint,type,count,dead)%>%
     rename(alive=count)%>%
     arrange(plug_number,type,timepoint)%>%
     group_by(plug_number,type)%>%
     mutate(nxt=lead(alive,n=1L))%>%
     rowwise()%>%
     mutate(alive=case_when(alive<nxt ~ nxt,TRUE ~as.numeric(alive)))
saveRDS(surv,"Data/survdata_endpoint")


unfold<-function(x,lost_colname,surv_colname){
     lost_expanded<-x[rep(row.names(x), x[[lost_colname]]), 1:ncol(x)]
     lost_final<-lost_expanded%>%mutate(outcome=0)
     surv_expanded<-x[rep(row.names(x), x[[surv_colname]]), 1:ncol(x)]
     surv_final<-surv_expanded %>% mutate(outcome=1)
     rbind(lost_final,surv_final)
}
dates<-read_tsv("Data/dates.txt")%>%clean_names()%>%select(-date)
surv_AH<-left_join(unfold(surv,"dead","alive"),dates,by="timepoint") #dead=0, alive=1
saveRDS(surv_AH,"Data/survdata_timeseries")



####################### FORMATTING BLEACHING ######################################################### #####
bl_counts<-raw%>%select(plug_number,temperature,conditioned,flow,growth_shade,noquote(order(colnames(raw))))%>%
     select(-contains("part"))%>%
     select(-contains("x"))%>%
     gather(measurement,count,-plug_number,-temperature,-conditioned,-flow,-growth_shade)%>%
     separate(measurement,into=c("measurement","date"),sep=-6)%>%
     mutate(measurement=case_when(measurement=="agg_"~"agg_surv",
                                  measurement=="ind_"~"ind_surv",
                                  measurement=="agg_bleach_"~"agg_bleach",
                                  measurement=="ind_bleach_"~"ind_bleach"))%>%
     separate(measurement,into=c("type","measurement"),sep="_")%>%
     mutate(timepoint=case_when(date=="071519"~1,
                                date=="071719"~1,
                                date=="071819"~1,
                                date=="072219"~2,
                                date=="072419"~2,
                                date=="072919"~3,
                                date=="073119"~3,
                                date=="080519"~4,
                                date=="080719"~4,
                                date=="081219"~5,
                                date=="081419"~5,
                                date=="081919"~6,
                                date=="082119"~6,
                                date=="091719"~7,
                                date=="091919"~7,
                                date=="101519"~8,
                                date=="101719"~8,
                                date=="111219"~9,
                                date=="111419"~9,
                                date=="121019"~10,
                                date=="121219"~10))%>%
     select(-temperature,-conditioned,-flow,-growth_shade,-date)%>%
     mutate(count=as.numeric(count))%>%
     group_by(plug_number,timepoint,type,measurement)%>%summarise(count=sum(count,na.rm=TRUE))%>%
     spread(measurement,count)%>%
     mutate(total=bleach+surv)%>%
     mutate(nonbleach=total-bleach)%>%
     select(-total,-surv)


bleach<-left_join(bl_counts,metadata,by="plug_number")%>%
     select(plug_number,temperature,conditioned,flow,growth_shade,timepoint,type,bleach,nonbleach)


bleach_AH<-left_join(unfold(bleach,"bleach","nonbleach"),dates,by="timepoint") #bleach=0, nonbleach=1
saveRDS(bleach_AH,"Data/bleachdata")


####################### SHADE DECLINE (F1) ########################################################### #####
setwd("./")
library(tidyverse);library(cowplot);library(plotrix)

raw<-readRDS("Data/survdata_timeseries")
raw$growth_shade <- factor(raw$growth_shade,levels = c("X2","X4"),labels=c("High","Low"))
quartz(w=(81/25.4),h=2)
ggplot(raw)+
     geom_vline(xintercept=45,linetype="dotted",color="gray")+
     theme_classic(base_size=8)+ylab("Survivorship")+xlab("Days Post Settlement")+
     scale_x_continuous(breaks=seq(0,150,30))+scale_y_continuous(breaks=seq(0,1,0.2))+
     geom_smooth(aes(days,outcome,color=growth_shade))+
     labs(color='Light Level')+
     annotate("text",label="light * time p<0.001",x=-Inf,y=-Inf,size=2,hjust=-0.1,vjust=-1,fontface = 'italic')

####################### SURVIVORSHIP MAIN EFFECTS (F2) ############################################### #####
library(tidyverse);library(cowplot);library(egg)
setwd("./")
raw<-readRDS("Data/survdata_endpoint")

data<-raw%>%filter(growth_shade=="X2")%>%filter(timepoint==10)%>%select(-nxt)%>%
     mutate(surv=alive/(alive+dead))%>%filter(surv!="NaN")
data$temperature <- factor(data$temperature,levels = c("Cold","Ambient","Hot"))
data$type <- factor(data$type,levels = c("agg","ind"),labels=c("Aggregate","Individual"))
data$flow <- factor(data$flow,levels = c("Yes","No"),labels=c("High","Low"))
data$conditioned <- factor(data$conditioned,levels = c("No","Yes"),labels=c("1 week","10 weeks"))

clean<-data%>%group_by(type)%>%summarise(mean=mean(surv,na.rm=TRUE),sd=sd(surv,na.rm=TRUE))
a<-ggplot(clean,aes(type,mean,fill=type,ymin=mean-sd,ymax=mean+sd))+
     geom_errorbar(aes(color=type),width=0)+
     geom_point(size=3,pch=21,color="black")+
     theme_classic(base_size=8)+
     xlab("ColonyType")+
     ylab("Survivorship")+
     scale_fill_manual(values=c("cyan","gray"))+
     scale_color_manual(values=c("cyan","gray"))+
     theme(legend.position="none")+
     scale_y_continuous(limits=c(-0.1,1),breaks=seq(0,1,0.2))+
     annotate("text",label="type p<0.001",x=-Inf,y=-Inf,size=2,hjust=-0.1,vjust=-1,fontface = 'italic');a


clean<-data%>%group_by(flow,type)%>%summarise(mean=mean(surv,na.rm=TRUE),sd=sd(surv,na.rm=TRUE))
b<-ggplot(clean,aes(flow,mean,fill=flow,ymin=mean-sd,ymax=mean+sd))+
     geom_errorbar(aes(color=flow),width=0)+
     geom_point(size=3,pch=21,color="black")+
     theme_classic(base_size=8)+
     facet_wrap(~type,nrow=2)+
     xlab("Flow")+
     ylab("Survivorship")+
     scale_fill_manual(values=c("orange","gray"))+
     scale_color_manual(values=c("orange","gray"))+
     theme(legend.position="none",
           axis.title.y=element_blank(),
           strip.background = element_blank(),
           strip.text.x = element_blank())+
     scale_y_continuous(limits=c(-0.2,1),breaks=seq(0,1,0.2));b

my_tag <- c("", "flow * type p<0.001")
b<-tag_facet(b, 
          x = -Inf, y = -Inf, 
          vjust = -1, hjust = -0.1,
          open = "", close = "",
          fontface = 'italic',
          size = 2,
          tag_pool = my_tag)

clean<-data%>%group_by(temperature,type)%>%summarise(mean=mean(surv,na.rm=TRUE),sd=sd(surv,na.rm=TRUE))
c<-ggplot(clean,aes(temperature,mean,fill=temperature,ymin=mean-sd,ymax=mean+sd))+
     geom_errorbar(aes(color=temperature),width=0)+
     geom_point(size=3,pch=21,color="black")+
     theme_classic(base_size=8)+
     facet_wrap(~type,nrow=2,strip.position="right")+
     xlab("Temperature")+
     ylab("Survivorship")+
     scale_fill_manual(values=c("Blue","Gray","Red"))+
     scale_color_manual(values=c("Blue","Gray","Red"))+
     theme(legend.position="none",axis.line.y=element_blank(),
           axis.text.y=element_blank(),
           axis.ticks.y=element_blank(),
           axis.title.y=element_blank(),
           strip.background = element_blank())+
     scale_y_continuous(limits=c(-0.2,1),breaks=seq(0,1,0.2));c
my_tag <- c("", "temp * type p<0.001")
c<-tag_facet(c, x = -Inf, y = -Inf, 
             vjust = -1, hjust = -0.1, open = "", close = "",  fontface = 'italic', size = 2,tag_pool = my_tag)


quartz(width=(105/25.4),height=2)
plot_grid(a,b,c,rel_widths=c(1.3,1,1.5),nrow=1,align="h",labels=c("A","B","C"),label_size=8,label_x=c(0.25,0.2,0.0))






####################### SURVIVORSHIP INTERACTIONS (F3) ############################################### ######
quartz(width=(81/25.4),height=2)


clean<-data%>%group_by(conditioned,type,flow)%>%summarise(mean=mean(surv,na.rm=TRUE),sd=sd(surv,na.rm=TRUE))
a<-ggplot(clean,aes(conditioned,mean,fill=flow,ymin=mean-sd,ymax=mean+sd))+
     geom_errorbar(aes(color=flow),position=position_dodge(width=1),width=0)+
     geom_point(size=3,position=position_dodge(width=1),color="black",pch=21)+
     theme_classic(base_size=8)+
     facet_wrap(~type,nrow=1)+
     scale_fill_manual(values=c("orange","gray"))+
     scale_color_manual(values=c("orange","gray"))+
     ylab("Survivorship")+
     xlab("Substrate Conditioning")+
     labs(fill = "Flow Rate")+
     labs(color = " Rate")+
     scale_y_continuous(limits=c(-0.15,1),breaks=seq(0,1,0.2))+
     coord_cartesian(clip = 'off')+
     guides(color=FALSE);a
tag_facet <- function(p, open = "(", close = ")", tag_pool = letters, x = -Inf, y = Inf, 
                      hjust = -0.5, vjust = 1.5, fontface = 2, family = "", ...) {
     
     gb <- ggplot_build(p)
     lay <- gb$layout$layout
     tags <- cbind(lay, label = paste0(open, tag_pool[lay$PANEL], close), x = x, y = y)
     p + geom_text(data = tags, aes_string(x = "x", y = "y", label = "label"), ..., hjust = hjust, 
                   vjust = vjust, fontface = fontface, family = family, inherit.aes = FALSE)
}
my_tag <- c("flow*type*cond p<0.001","")
tag_facet(a, 
             x = -Inf, y = -Inf, 
          vjust = -1, hjust = -0.1,
          open = "", close = "",
             fontface = 'italic',
             size = 2,
             tag_pool = my_tag)


####################### BLEACHING EFFECTS (F4) ####################################################### #####
setwd("./")
library(plotly)
data<-readRDS("Data/bleachdata")%>%filter(growth_shade=="X2")%>%
     mutate(outcome=case_when((timepoint==1|timepoint==2)~1,TRUE ~as.numeric(outcome)))%>%
     mutate(outcome=case_when(outcome==1 ~0,
                              outcome==0 ~1))

data$temperature <- factor(data$temperature,levels = c("Cold","Ambient","Hot"))
data$type <- factor(data$type,levels = c("agg","ind"),labels=c("Aggregate","Individual"))
data$flow <- factor(data$flow,levels = c("Yes","No"),labels=c("High","Low"))
data$conditioned <- factor(data$conditioned,levels = c("No","Yes"),labels=c("1 week","10 weeks"))

a<-ggplot(data)+geom_smooth(aes(days,outcome,color=type), method = "gam", formula = y ~ poly(-x,2))+
     theme_classic(base_size=8)+
     ylab("Bleaching Proportion")+
     xlab("Days Post Settlement")+
     scale_fill_manual(values=c("cyan","gray"))+
     scale_color_manual(values=c("cyan","gray"))+
     scale_x_continuous(breaks=seq(0,150,30))+
     coord_cartesian(ylim=c(0.0, 0.25))+
     theme(legend.position=c(0.5,0.92),legend.key.size = unit(0.5,"line"))+
     labs(color="Colony Type")+
     annotate("text",label="type p<0.001",x=-Inf,y=-Inf,size=2,hjust=-0.1,vjust=-1,fontface = 'italic');a

b<-ggplot(data)+geom_smooth(aes(days,outcome,color=flow), method = "gam", formula = y ~ poly(-x,2))+
     theme_classic(base_size=8)+
     xlab("Days Post Settlement")+
     scale_fill_manual(values=c("orange","gray"))+
     scale_color_manual(values=c("orange","gray"))+
     scale_x_continuous(breaks=seq(0,150,30))+
     coord_cartesian(ylim=c(0.0, 0.25))+
     theme(legend.position=c(0.5,0.92),axis.line.y=element_blank(),
           axis.text.y=element_blank(),
           axis.ticks.y=element_blank(),
           axis.title.y=element_blank(),legend.key.size = unit(0.5,"line"))+
     labs(color="Flow")+
     annotate("text",label="flow p<0.001",x=-Inf,y=-Inf,size=2,hjust=-0.1,vjust=-1,fontface = 'italic');b


c<-ggplot(data)+geom_smooth(aes(days,outcome,color=temperature), method = "gam", formula = y ~ poly(-x,2))+
     theme_classic(base_size=8)+
     xlab("Days Post Settlement")+
     scale_fill_manual(values=c("Blue","Gray","Red"))+
     scale_color_manual(values=c("Blue","Gray","Red"))+
     scale_x_continuous(breaks=seq(0,150,30))+
     coord_cartesian(ylim=c(0.0, 0.25))+
     facet_wrap(~type,nrow=2)+
     ylab("Bleaching Proportion")+
     theme(legend.position=c(0.5,0.9),
           legend.key.size = unit(0.5,"line"),
           strip.background = element_blank(),
           strip.text.x = element_blank())+
     labs(color="Temperature");c
my_tag <- c("", "temp * type p=0.0027")
c<-tag_facet(c, x = -Inf, y = -Inf, 
             vjust = -1, hjust = -0.1, open = "", close = "",  fontface = 'italic', size = 2,tag_pool = my_tag)

d<-ggplot(data)+geom_smooth(aes(days,outcome,color=conditioned), method = "gam", formula = y ~ poly(-x,2))+
     theme_classic(base_size=8)+
     xlab("Days Post Settlement")+
     scale_color_manual(values=c("gray","purple"))+
     scale_x_continuous(breaks=seq(0,150,30))+
     coord_cartesian(ylim=c(0.0, 0.25))+
     facet_wrap(~type,nrow=2)+
     theme(legend.position=c(0.5,0.92),axis.line.y=element_blank(),
           axis.text.y=element_blank(),
           axis.ticks.y=element_blank(),
           axis.title.y=element_blank(),
           legend.key.size = unit(0.5,"line"),
           strip.background = element_blank(),
           strip.text.x = element_blank())+
     labs(color="Conditioned");d
my_tag <- c("", "temp * type p=0.0207")
d<-tag_facet(d, x = -Inf, y = -Inf, 
             vjust = -1, hjust = -0.1, open = "", close = "",  fontface = 'italic', size = 2,tag_pool = my_tag)


quartz(width=(169/25.4),height=2.5)
plot_grid(a,b,c,d,nrow=1,rel_widths=c(1.3,1,1.3,1),labels=c("A","B","C","D"),label_size=8,label_x=c(0.215,0,0.21,0))

####################### GROWTH EFFECTS (F5,F6) ####################################################### #####
setwd("./")
data<-readRDS("Data/growthdata")
data$temperature <- factor(data$temperature,levels = c("Cold","Ambient","Hot"))
data$type <- factor(data$type,levels = c("agg","ind"),labels=c("Aggregate","Individual"))
data$flow <- factor(data$flow,levels = c("Yes","No"),labels=c("High","Low"))
data$conditioned <- factor(data$conditioned,levels = c("No","Yes"),labels=c("1 week","10 weeks"))

y<-ggplot(data)+geom_boxplot(aes(type,growth,fill=type))+
     theme_classic(base_size=8)+
     scale_fill_manual(values=c("cyan","gray"))+
     ylab("Growth Rate (% change)")+
     xlab("")+
     theme(legend.position="none")+
     annotate("text",label="type p=0.0012",x=-Inf,y=-Inf,size=2,hjust=-0.1,vjust=-1,fontface = 'italic');y

z<-ggplot(data)+geom_boxplot(aes(type,final_december,fill=type))+
     theme_classic(base_size=8)+
     ylab(bquote('Final Size ('~ mm^2*')'))+
     scale_fill_manual(values=c("cyan","gray"))+
     xlab("")+
     theme(legend.position="none")+
     annotate("text",label="type p<0.001",x=-Inf,y=-Inf,size=2,hjust=-0.1,vjust=-1,fontface = 'italic');z

quartz(width=(81/25.4),height=2)
plot_grid(y,z,nrow=1,labels=c("A","B"),label_size=8,label_x=c(0.25,0.21),align="h")

b<-ggplot(data)+geom_hline(yintercept=0,color="darkgray",linetype="dotted")+
     geom_boxplot(aes(flow,growth,fill=type))+
     theme_classic(base_size=8)+
     scale_fill_manual(values=c("cyan","gray"),guid=guide_legend(nrow=2))+
     ylab("Growth Rate (% change)")+
     xlab("Flow Rate")+
     labs(fill="Type")+
     theme(legend.position="bottom",
           legend.key.size = unit(0.75,"line"))+
     scale_y_continuous(limits=c(-100,250),breaks=seq(-100,250,50))+
     annotate("text",label="flow*type p<0.001",x=-Inf,y=-Inf,size=2,hjust=-0.1,vjust=-1,fontface = 'italic')+ 
     guides(fill = guide_legend(nrow = 1));b

c<-ggplot(data)+geom_hline(yintercept=0,color="darkgray",linetype="dotted")+
     geom_boxplot(aes(temperature,growth,fill=conditioned))+
     ylab("Growth Rate (% change)")+
     scale_fill_manual(values=c("gray","purple"),guid=guide_legend(nrow=2))+
     theme_classic(base_size=8)+
     theme(legend.position="bottom",axis.line.y=element_blank(),
           axis.text.y=element_blank(),
           axis.ticks.y=element_blank(),
           axis.title.y=element_blank(),,
           legend.key.size = unit(0.75,"line"))+
     labs(fill="Conditioned")+
     xlab("Temperature")+
     scale_y_continuous(limits=c(-100,250),breaks=seq(-100,250,50))+
     annotate("text",label="temp *cond p=0.0256",x=-Inf,y=-Inf,size=2,hjust=-0.1,vjust=-1,fontface = 'italic')+ 
     guides(fill = guide_legend(nrow = 1));c

quartz(width=(105/25.4),height=2)
plot_grid(b,c,nrow=1,labels=c("A","B"),label_size=8,label_x=c(0.2,0.05),align="h")




####################### CORRELATION (SF5) ############################################################ #####
library(tidyverse);library(egg)
setwd("./")
growth<-readRDS("Data/growthdata")
growth$temperature <- factor(growth$temperature,levels = c("Cold","Ambient","Hot"))
growth$type <- factor(growth$type,levels = c("agg","ind"),labels=c("Aggregate","Individual"))
growth$flow <- factor(growth$flow,levels = c("Yes","No"),labels=c("High","None"))
growth_out<-growth%>%group_by(temperature,flow,conditioned,type)%>%summarise(growth=mean(growth))%>%select(growth)

surv<-readRDS("Data/survdata_endpoint")%>%filter(growth_shade=="X2")%>%filter(timepoint==10)%>%select(-nxt)%>%
     mutate(surv=alive/(alive+dead))%>%filter(surv!="NaN")
surv$temperature <- factor(surv$temperature,levels = c("Cold","Ambient","Hot"))
surv$type <- factor(surv$type,levels = c("agg","ind"),labels=c("Aggregate","Individual"))
surv$flow <- factor(surv$flow,levels = c("Yes","No"),labels=c("High","None"))
surv_out<-surv%>%group_by(temperature,flow,conditioned,type)%>%summarise(surv=mean(surv))

data<-bind_cols(surv_out,growth_out)

a<-ggplot(data)+geom_point(aes(growth,surv,color=type))+
     geom_smooth(aes(growth,surv,color=type),method="lm")+
     scale_y_continuous(limits=c(0,1),breaks=seq(0,1,0.2))+
     scale_x_continuous(limits=c(-20,50),breaks=seq(-20,50,10))+
     theme_classic()+
     facet_wrap(~type)+
     ylab("Survivorship")+
     xlab("Growth Rate")+
     scale_color_manual(values=c("cyan","gray"))+
     theme(legend.position="none");a

tag_facet <- function(p, open = "(", close = ")", tag_pool = letters, x = -Inf, y = Inf, 
                      hjust = -0.5, vjust = 1.5, fontface = 2, family = "", ...) {
     
     gb <- ggplot_build(p)
     lay <- gb$layout$layout
     tags <- cbind(lay, label = paste0(open, tag_pool[lay$PANEL], close), x = x, y = y)
     p + geom_text(data = tags, aes_string(x = "x", y = "y", label = "label"), ..., hjust = hjust, 
                   vjust = vjust, fontface = fontface, family = family, inherit.aes = FALSE)
}
my_tag <- c("p=0.2096", "p=0.1404")
quartz(width=3.5,height=2)

tag_facet(a, x = -Inf, y = -Inf, 
             vjust = -1, hjust = -0.1, open = "", close = "",  fontface = 'italic', size = 2,tag_pool = my_tag)




        
####################### OPTIMIZATION ################################################################# #####
#determines best treatment combinations for different metrics
library(tidyverse);library(cowplot);library(egg);library(plotrix)
setwd("./")
raw<-readRDS("Data/survdata_endpoint")

#calculate treatment with highest average survivorship
data<-raw%>%filter(growth_shade=="X2")%>%filter(timepoint==10)%>%select(-nxt)%>%
     mutate(surv=alive/(alive+dead))%>%filter(surv!="NaN")%>%group_by(temperature,conditioned,flow)%>%
     summarise(mean=mean(surv))

#calculate treatment with highest average growth
data<-readRDS("Data/growthdata")%>%group_by(temperature,conditioned,flow)%>%
     summarise(mean=mean(change))

#mean and se of growth overall
readRDS("Data/growthdata")%>%summarise(mean=mean(growth),se=std.error(growth))

####################### CONICALS (SF2) ############################################################### #####
#data for larval conical temperatures
setwd("Data/tempdata")
library(readxl);library(tidyverse);library(lubridate);library(scales);library(plotrix)
hot<-read_excel("conicals.xlsx",sheet="Hot");str(data)
ambient<-read_excel("conicals.xlsx",sheet="Ambient");str(ambient)
cold<-read_excel("conicals.xlsx",sheet="Cold");str(cold)

data<-full_join(full_join(hot,ambient,by="Date"),cold,by="Date")%>%
     rename(Hot=Temp.x,Ambient=Temp.y)%>%
     gather(Treatment,Temp,-Date)%>%
     filter(Date>as.POSIXct("2019-07-06")&Date<as.POSIXct("2019-07-9"))
data$Treatment <- factor(data$Treatment,levels = c("Cold", "Ambient", "Hot"))

quartz(h=2,w=5)
ggplot(data)+geom_point(aes(Date,Temp,color=Treatment),alpha=0.3,size=1)+
     theme_classic()+
     scale_color_manual(values=c("Blue","Gray","Red"))+
     geom_smooth(aes(Date,Temp,color=Treatment),se=FALSE,span=0.15,size=0.5)+
     scale_x_datetime(date_breaks="1 day",minor_breaks=waiver(),labels=date_format("%m-%d"))+
     scale_y_continuous(breaks=seq(23,30,1))+
     ylab("Temperature (°C)")+
     theme(legend.key.size = unit(0.5,"line"))

#mean and se of temperature during larval rearing 
data%>%group_by(Treatment)%>%summarise(mean=mean(Temp),se=std.error(Temp))


####################### TEMP (SF 3) ################################################################## #####
setwd("Data/tempdata")
library(readxl);library(tidyverse);library(lubridate);library(scales);library(plotrix)
a<-read_xlsx("1_2x_Flow.xlsx");b<-read_xlsx("2_HOBO.xlsx",sheet="2x_Flow"); as.numeric(b$Light)->b$Light;c<-read_xlsx("3_2x_Flow.xlsx");F2<-bind_rows(a,b,c)%>%rename(F2_Temp=Temp,F2_Light=Light)%>%mutate(F2_Temp=F2_Temp+0.004)
a<-read_xlsx("1_4x_Flow.xlsx");b<-read_xlsx("2_HOBO.xlsx",sheet="4x_Flow"); as.numeric(b$Light)->b$Light;c<-read_xlsx("3_4x_Flow.xlsx");F4<-bind_rows(a,b,c)%>%rename(F4_Temp=Temp,F4_Light=Light)%>%mutate(F4_Temp=F4_Temp-0.021)
a<-read_xlsx("1_2x_No.xlsx");b<-read_xlsx("2_HOBO.xlsx",sheet="2x_No"); as.numeric(b$Light)->b$Light;c<-read_xlsx("3_2x_No.xlsx");N2<-bind_rows(a,b,c)%>%rename(N2_Temp=Temp,N2_Light=Light)%>%mutate(N2_Temp=N2_Temp-0.009)
a<-read_xlsx("1_4x_No.xlsx");b<-read_xlsx("2_HOBO.xlsx",sheet="4x_No"); as.numeric(b$Light)->b$Light;c<-read_xlsx("3_4x_No.xlsx");N4<-bind_rows(a,b,c)%>%rename(N4_Temp=Temp,N4_Light=Light)%>%mutate(N4_Temp=N4_Temp+0.064)
field<-read_delim("NBDC_data2.txt",delim=" ")%>%
     mutate_if(is.character, as.numeric)%>%
     select(YY,MM,DD,hh,mm,WTMP)%>%
     rename(Field=WTMP)%>%
     separate(YY,into=c("trash","YY"),sep=2)%>%
     select(-trash)%>%
     unite(date,YY,MM,DD,sep="-")%>%
     filter(mm==0)%>%
     unite(date2,hh,mm,sep=":")%>%
     unite(x,date, date2, sep=" ")%>%
     mutate(Date=ymd_hm(x))%>%
     filter(Field!=999.0)%>%select(-x)

temp<-full_join(full_join(F2,F4,by="Date"),full_join(N2,N4,by="Date"),by="Date")%>%
     select(Date,F2_Temp,F4_Temp,N2_Temp,N4_Temp)%>%mutate(Date=ymd_hms(Date))%>%
     mutate(Date=round_date(Date, unit = "hour"))

figtemp<-full_join(field,temp,by="Date")%>%select(Date,everything())%>%     
     rowwise()%>%
     gather(treatment,temperature,-Date)%>%
     filter(Date>="2019-6-1")
figtemp$treatment <- factor(figtemp$treatment ,levels=c("Field","F2_Temp","N2_Temp","F4_Temp","N4_Temp"),labels=c("NBDC MOKH1 (Field)"," High-Light High-Flow","High-Light Low-Flow","Low-Light High-Flow","Low-Light Low-Flow"))

quartz(w=6,h=3)
ggplot(figtemp)+
     geom_point(aes(Date,temperature,color=treatment),alpha=0.1,size=0.5)+
     geom_smooth(aes(Date,temperature,color=treatment),method="loess",span=0.1 ,se=TRUE,size=0.5)+
     theme_classic(base_size=10)+
     ylab("Temperature (°C)")+
     scale_x_datetime(date_breaks="1 month",minor_breaks=waiver(),labels=date_format("%b-%y"))+
     scale_y_continuous(breaks=seq(23,30,1))+
     theme(legend.key.size = unit(0.7,"line"),
           legend.title=element_blank(),
           legend.position="top",
           legend.text = element_text(size=6))+
     annotate("text",x=as_datetime("2019-6-1"),label="Mean ± 1SE (grow-out phase only)",y=25,color="black",size=2,alpha=0.8,hjust=0)+
     annotate("text",x=as_datetime("2019-6-1"),label="Field: 27.1 ± (0.03)°C",y=24.6,color="#F8766D",size=2.5,alpha=0.8,hjust=0,fontface="bold")+
     annotate("text",x=as_datetime("2019-6-1"),label="HLHF: 27.5 ± (0.03)°C",y=24.2,color="#A3A500",size=2.5,alpha=0.8,hjust=0,fontface="bold")+
     annotate("text",x=as_datetime("2019-6-1"),label="HLLF: 27.3 ± (0.03)°C",y=23.8,color="#00BF7D",size=2.5,alpha=0.8,hjust=0,fontface="bold")+
     annotate("text",x=as_datetime("2019-6-1"),label="LLHF: 27.3 ± (0.03)°C",y=23.4,color="#00B0F6",size=2.5,alpha=0.8,hjust=0,fontface="bold")+
     annotate("text",x=as_datetime("2019-6-1"),label="LFLL: 27.4 ± (0.03)°C",y=23,color="#E76BF3",size=2.5,alpha=0.8,hjust=0,fontface="bold")+
     annotate("text",x=as_datetime("2020-1-1"),label="MMM 2008-2012",y=27.7,size=2,alpha=0.8,hjust=0)+
     geom_hline(yintercept=27.5,linetype="dotted",color="gray")

show_col(hue_pal()(5))
#mean and se temperatures for larval growout phase
figtemp%>%filter(Date>="2019-7-25")%>%group_by(treatment)%>%summarise(mean=mean(temperature,na.rm=TRUE),se=std.error(temperature))

#dhw formatting
DHW<-left_join(field,temp,by="Date")%>%select(Date,everything())%>%
     filter(Date>="2019-7-20")%>%
     filter(Date<="2020-1-15")%>%
     rowwise()%>%
     mutate(mean=mean(F2_Temp,F4_Temp,N2_Temp,N4_Temp,na.rm=TRUE))%>%select(Date,Field,mean)%>%
     mutate(diff=mean-Field)%>%
     drop_na()%>%
     mutate(dhh=mean-28.5)%>%
     filter(dhh>=0)

#degree heating week calculations 
sum(DHW$dhh)/24/7
#mean temperature diference between tanks and field
mean(DHW$diff)
#mean temperature difference betwween tanks
full_join(field,temp,by="Date")%>%select(Date,everything())%>%     
     rowwise()%>%
     mutate(F=mean(F2_Temp,F4_Temp,na.rm=TRUE))%>%
     mutate(N=mean(N2_Temp,N4_Temp,na.rm=TRUE))%>%
     filter(Date>="2019-7-20")%>%
     select(Date,F,N)%>%
     gather(tank,temp,-Date)%>%group_by(tank)%>%summarise(mean=mean(temp,na.rm=TRUE),se=std.error(temp))
     
####################### LIGHT (SF4) ################################################################## #####
setwd("Data/lightdata")
library(tidyverse);library(readxl);library(lubridate);library(cowplot)

n2<-read_excel("2x_No_Flow.xlsx",skip=1)%>%select(-'#',-5)%>%rename("Date"=1,"Temp"=2,"n2"=3)%>%
     mutate(Date=ymd_hms(Date))%>%select(-Temp)%>%head(275)
f2<-read_excel("2x_Flow.xlsx",skip=1)%>%select(-'#',-5)%>%rename("Date"=1,"Temp"=2,"f2"=3)%>%
     mutate(Date=ymd_hms(Date))%>%select(-Temp)%>%select(-Date)%>%head(275)
n4<-read_excel("4x_No_Flow.xlsx",skip=1)%>%select(-'#',-5)%>%rename("Date"=1,"Temp"=2,"n4"=3)%>%
     mutate(Date=ymd_hms(Date))%>%select(-Temp)%>%select(-Date)%>%head(275)
f4<-read_excel("4x_Flow.xlsx",skip=1)%>%select(-'#',-5)%>%rename("Date"=1,"Temp"=2,"f4"=3)%>%
     mutate(Date=ymd_hms(Date))%>%select(-Temp)%>%select(-Date)%>%head(275)

nft1<-read_excel("No_Flow_Test_#1.xlsx",skip=1)%>%select(-'#',-5)%>%rename("Date"=1,"Temp"=2,"nft1"=3)%>%
     mutate(Date=ymd_hms(Date))%>%select(-Temp)%>%select(-Date)%>%head(275)
nft2<-read_excel("No_Flow_Test_#2.xlsx",skip=1)%>%select(-'#',-5)%>%rename("Date"=1,"Temp"=2,"nft2"=3)%>%
     mutate(Date=ymd_hms(Date))%>%select(-Temp)%>%select(-Date)%>%head(275)
ft1<-read_excel("Flow_Test_#1.xlsx",skip=1)%>%select(-'#',-5)%>%rename("Date"=1,"Temp"=2,"ft1"=3)%>%
     mutate(Date=ymd_hms(Date))%>%select(-Temp)%>%select(-Date)%>%head(275)
ft2<-read_excel("Flow_Test_#2.xlsx",skip=1)%>%select(-'#',-5)%>%rename("Date"=1,"Temp"=2,"ft2"=3)%>%
     mutate(Date=ymd_hms(Date))%>%select(-Temp)%>%select(-Date)%>%head(275)


a<-bind_cols(n2,f2,n4,f4,nft1,nft2,ft1,ft2)%>%
     mutate(Date=ymd_hms(Date))%>%
     mutate(hour=hour(Date))%>%
     mutate(day=date(Date))%>%
     filter(day=="2020-04-02")%>%
     rowwise()%>%
     mutate(high=mean(c(f2,ft1,ft2,nft1,nft2)))%>%
     mutate(low=mean(c(n4,f4)))%>%
     mutate(hour=hour(Date))%>%
     select(hour,high,low)%>%
     gather(tank,light,-hour)
a$tank<-factor(a$tank,labels=c("High Light","Low Light"))

b<-bind_cols(n2,f2,n4,f4,nft1,nft2,ft1,ft2)%>%
     mutate(Date=ymd_hms(Date))%>%
     mutate(hour=hour(Date))%>%
     mutate(day=date(Date))%>%
     filter(day=="2020-04-02")%>%
     rowwise()%>%
     mutate(meanF=mean(c(f2,ft1,ft2)))%>%
     mutate(meanN=mean(c(nft1,nft2)))%>% #exclude n2, bad readings
     mutate(hour=hour(Date))%>%
     select(meanF,meanN,f4,n4,hour)%>%
     gather(tank,light,-hour)
b$tank<-factor(b$tank,levels=c("meanF","meanN","f4","n4"),labels=c("High-Light High-Flow","High-Light Low-Flow","Low-Light High-Flow","Low-Light Low-Flow"))

#daily light dose
b%>%group_by(tank)%>%summarise(int=sum(light)) #daily integral
#difference between light treatments
mean(c(19569,24951))/mean(c(93933,96774))#light in shade tanks is 23% of high
#percent difference in daily light dose between tanks
(96774-93933)/96774 #light in high flow tank is 2.9% less than in low flow tank

#percent difference in maximum intensity (between 10AM-2PM) between tanks
b%>%filter(hour>10&hour<12)%>%group_by(tank)%>%summarise(mean=mean(light)) #max
(2607-2209)/2607 #peak values are ~15% less in high flow tank

c<-ggplot(a)+
     geom_smooth(aes(hour,light,color=tank),span=0.3)+
     theme_classic(base_size=8)+
     ylab("Light Intensity (lux)")+
     xlab("Time of Day (hour)")+
     scale_x_continuous(breaks=seq(0,24,2))+
     scale_y_continuous(breaks=seq(0,3000,500))+
     theme(legend.title=element_blank(),legend.position="bottom",
           legend.key.size = unit(0.75,"line"))
d<-ggplot(b)+
     geom_smooth(aes(hour,light,color=tank),span=0.3)+
     theme_classic(base_size=8)+
     ylab("Light Intensity (lux)")+
     xlab("Time of Day (hour)")+
     scale_x_continuous(breaks=seq(0,24,2))+
     scale_y_continuous(breaks=seq(0,3000,500))+
     theme(legend.title=element_blank(),legend.position="bottom",
           legend.key.size = unit(0.75,"line"),
           axis.line.y=element_blank(),
           axis.text.y=element_blank(),
           axis.ticks.y=element_blank(),
           axis.title.y=element_blank())+
     scale_color_manual(values=c("#A3A500","#00BF7D","#00B0F6","#E76BF3"))+ guides(colour = guide_legend(nrow = 2))+
     annotate("text",x=14,label="Daily Light Integral",y=3200,color="black",size=2.5,alpha=0.8,hjust=0)+
     annotate("text",x=14,label="HLHF:93.9 klux",y=3000,color="#A3A500",size=2.5,alpha=0.8,hjust=0,fontface="bold")+
     annotate("text",x=14,label="HLLF:96.7 klux",y=2800,color="#00BF7D",size=2.5,alpha=0.8,hjust=0,fontface="bold")+
     annotate("text",x=14,label="LLHF:19.5 klux",y=2600,color="#00B0F6",size=2.5,alpha=0.8,hjust=0,fontface="bold")+
     annotate("text",x=14,label="LLLF:24.9 klux",y=2400,color="#E76BF3",size=2.5,alpha=0.8,hjust=0,fontface="bold")

quartz(w=5,h=2.75)
plot_grid(c,d,rel_widths=c(1.2,1),labels=c("A","B"),label_size=8,label_x=c(0.16,0.05),align="h")







