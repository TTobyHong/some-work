# Remove all lists
rm(list=ls(all=T))

a=flights

library(tidyverse)
library(nycflights13)

#Q1 UA flew the most flight
ggplot(data=flights,mapping=aes(x=carrier))+
  geom_bar()

#Q2
flights %>%
  group_by(dest) %>%
  summarise(prop_early = mean(arr_delay < 0, na.rm=TRUE),
            MDis = median(distance, na.rm=TRUE),
            count = n()) %>% 
  ggplot(aes(x=MDis,y=prop_early)) + geom_point(aes(size=count), alpha=1/2) +
  geom_smooth()

flights %>%
  group_by(dest) %>% summarise(prop_earlier=mean(arr_delay<0, na.rm=TRUE),
                               Median_distance=median(distance,na.rm=TRUE))

#Q3 Histogram
flights %>%
  group_by(carrier) %>% summarise(prop_earlier=mean(arr_delay<0,na.rm=TRUE)) %>%
  ggplot(aes(x=carrier,y=prop_earlier)) + geom_col()

flights %>%
  group_by(carrier) %>% 
  summarise(median_earlier=-median(arr_delay,na.rm=TRUE)) %>%
  ggplot(aes(x=carrier,y=median_earlier)) + geom_col()

#Part2
#Q4
data1<-read.csv("NavajoWaterExport.csv",header=T)
data2 <- data1
ggplot(data1, aes(x=Amount.of.Radium228)) + geom_bar(stat="bin",bins=10) +
  facet_wrap(~ Which.EPA.Section.is.This.From.)

data1 %>% 
  mutate(Amount.of.Radium228=ifelse(Amount.of.Radium228<0,0,Amount.of.Radium228))%>%
  ggplot(aes(x=Amount.of.Radium228)) + geom_histogram(binwidth = 0.4) +
  facet_wrap(~ Which.EPA.Section.is.This.From.)

#Q5
(df1 <- data1%>%
  select(Which.EPA.Section.is.This.From.,US.EPA.Risk.Rating,Amount.of.Uranium238)%>%
  filter(US.EPA.Risk.Rating!="Unknown Risk")%>%
  group_by(Which.EPA.Section.is.This.From.,US.EPA.Risk.Rating)%>%
  summarise(number.of.sites=n(),Mean_U238=mean(Amount.of.Uranium238,na.rm=TRUE)))


df1%>%
  group_by(US.EPA.Risk.Rating)%>%
  ggplot(aes(x=Which.EPA.Section.is.This.From.,y=number.of.sites,color=US.EPA.Risk.Rating)) +
           geom_bar(stat="identity",position = "dodge")
df1%>%
  ggplot(aes(x=Which.EPA.Section.is.This.From.,y=Mean_U238,color=US.EPA.Risk.Rating)) +
    geom_bar(stat="identity",position = "dodge")

#Section2 has the most sites with "More Risk"
#Section2 has the highest concentration of U_238

#Q6
library(maps)
library(measurements)
four_corners <- map_data("state",
                         region=c("arizona", "new mexico",
                                  "utah",
                                  "colorado"))
Sites=data1%>%
  transmute(long=conv_unit(Longitude,"deg_min_sec","dec_deg"),lat=conv_unit(Latitude,"deg_min_sec","dec_deg"),
         group=Which.EPA.Section.is.This.From.,
         Uranium=ifelse(Amount.of.Uranium238<0,0,Amount.of.Uranium238))
Sites$long <- -abs(as.numeric(Sites$long))
Sites$lat <- round(as.numeric(Sites$lat),digits=5)

ggplot(four_corners) + geom_polygon(mapping=aes(x=long,
                                                y=lat,
                                                group=group),fill=NA,
                                    color="black")+
  coord_map()+

  geom_point(data=Sites,mapping=aes(x=long,
                           y=lat,color=group,size=Uranium))+
  xlab("Longitude")+ylab("Latitude")

#Part3
#Q7
df3<- read_csv("CRDC2013_14_SCH.csv",na=c("-2","-5","-9"))
dfB=df3%>%
#Filter out the missing value
#  filter(TOT_DISCWODIS_EXPZT_M>=0,TOT_DISCWODIS_EXPZT_F>=0,TOT_DISCWDIS_EXPZT_IDEA_M>=0,TOT_DISCWDIS_EXPZT_IDEA_F>=0)%>%
#  filter(SCH_DISCWDIS_EXPZT_IDEA_BL_M>=0,SCH_DISCWDIS_EXPZT_IDEA_BL_F>=0,SCH_DISCWODIS_EXPZT_BL_M>=0,SCH_DISCWODIS_EXPZT_BL_F>=0)%>%
#  filter(TOT_DISCWODIS_EXPZT_M+TOT_DISCWODIS_EXPZT_F+TOT_DISCWDIS_EXPZT_IDEA_M+TOT_DISCWDIS_EXPZT_IDEA_F>0)%>%
  transmute(TS=TOT_ENR_M+TOT_ENR_F,TBS=SCH_ENR_BL_M+SCH_ENR_BL_F,
            SRE=TOT_DISCWODIS_EXPZT_M+TOT_DISCWODIS_EXPZT_F+TOT_DISCWDIS_EXPZT_IDEA_M+TOT_DISCWDIS_EXPZT_IDEA_F,
            BSRE=SCH_DISCWDIS_EXPZT_IDEA_BL_M+SCH_DISCWDIS_EXPZT_IDEA_BL_F+SCH_DISCWODIS_EXPZT_BL_M+SCH_DISCWODIS_EXPZT_BL_F,
            Prop_B=TBS/TS,Prop_BSRE=BSRE/SRE)
dfB%>%
  ggplot()+geom_point(aes(x=Prop_B,y=Prop_BSRE),alpha=1/2)+geom_smooth(aes(x=Prop_B,y=Prop_BSRE))
dfB%>%
  summarise(Prop_overallB=sum(TBS,na.rm=TRUE)/sum(TS,na.rm=TRUE),Prop_overallBRE=sum(BSRE,na.rm=TRUE)/sum(SRE,na.rm=TRUE))

#8
dfH=df3%>%
# filter(TOT_GTENR_M>=0,TOT_GTENR_F>=0,SCH_GTENR_HI_M>=0,SCH_GTENR_HI_F>=0)%>%
#  filter(TOT_ENR_M>=0,TOT_ENR_F>=0,SCH_ENR_HI_M>=0,SCH_ENR_HI_F>=0)%>%
  transmute(TS=TOT_ENR_M+TOT_ENR_F,TSH=SCH_ENR_HI_M+SCH_ENR_HI_F,
            SGT=TOT_GTENR_M+TOT_GTENR_F,HSGT=SCH_GTENR_HI_M+SCH_GTENR_HI_F,
            Prop_H=TSH/TS,Prop_HGT=HSGT/SGT)
dfH%>%
  ggplot()+geom_point(aes(x=Prop_H,y=Prop_HGT),alpha=1/20)+geom_smooth(aes(x=Prop_H,y=Prop_HGT))
dfH%>%
  summarise(Prop_overallH=sum(TSH,na.rm=TRUE)/sum(TS,na.rm=TRUE),Prop_overallHSGT=sum(HSGT,na.rm=TRUE)/sum(SGT,na.rm=TRUE))

#9
dfD=df3%>%
  filter(TOT_ENR_M+TOT_ENR_F>=SCH_ENR_IDEA_M+SCH_ENR_IDEA_F+
           SCH_ENR_504_M+SCH_ENR_504_F)%>%
  transmute(TS=TOT_ENR_M+TOT_ENR_F,TIDEA=SCH_ENR_IDEA_M+SCH_ENR_IDEA_F+SCH_ENR_504_M+SCH_ENR_504_F,
            DS_RLE=TOT_DISCWDIS_REF_IDEA_M+TOT_DISCWDIS_REF_IDEA_F,
            TS_RLE=TOT_DISCWDIS_REF_IDEA_M+TOT_DISCWDIS_REF_IDEA_F+
              TOT_DISCWODIS_REF_M+TOT_DISCWODIS_REF_F,
            Prop_D=TIDEA/TS,Prop_D_RLE=DS_RLE/TS_RLE)
dfD%>%
  ggplot()+geom_point(aes(x=Prop_D,y=Prop_D_RLE),alpha=1/5)+geom_smooth(aes(x=Prop_D,y=Prop_D_RLE))
dfD%>%
  summarise(Prop_overall_Dis=sum(TIDEA,na.rm=TRUE)/sum(TS,na.rm=TRUE),Prop_overall_Dis_CP=sum(DS_RLE,na.rm=TRUE)/sum(TS_RLE,na.rm=TRUE))


#10
dfA=df3%>%
  transmute(TS=TOT_ENR_M+TOT_ENR_F,TSA=SCH_ENR_AS_M+SCH_ENR_AS_F,
            SGT=TOT_GTENR_M+TOT_GTENR_F,ASGT=SCH_GTENR_AS_M+SCH_GTENR_AS_F,
            Prop_A=TSA/TS,Prop_AGT=ASGT/SGT)
dfA%>%
  ggplot()+geom_point(aes(x=Prop_A,y=Prop_AGT),alpha=1/5)+geom_smooth(aes(x=Prop_A,y=Prop_AGT))
dfA%>%
  summarise(Prop_overall_Asian=sum(TSA,na.rm=TRUE)/sum(TS,na.rm=TRUE),Prop_overall_AsianinGT=sum(ASGT,na.rm=TRUE)/sum(SGT,na.rm=TRUE))
