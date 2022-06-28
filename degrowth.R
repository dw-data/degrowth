setwd("C:/Users/ajitn/Documents/R")
library(dplyr)
library(ggplot2)
library(gapminder)
library(readr)
library(gganimate)
library(gifski)
library(av)
library(png)
library(readr)
library(directlabels)
library(ggtext)
library(grid)
library(scales)
library(tidytext)
library(svglite)

#Gather Data

#Country codes from https://gist.github.com/tadast/8827699
countrycodes<-read.csv("countries_codes_and_coordinates.csv") %>% 
  select(Country,Alpha.3.code)

#Human Development Index from https://ourworldindata.org/grapher/human-development-index-vs-gdp-per-capita
data0<-read.csv("human-development-index-vs-gdp-per-capita.csv", header = TRUE, stringsAsFactors=FALSE, na.strings = c("", "NA")) 

#CO2 emissions and GDP-per-capita from https://ourworldindata.org/co2-gdp-decoupling
data1<-read.csv("co2-emissions-and-gdp-per-capita.csv", header = TRUE, stringsAsFactors = FALSE)
data2<-right_join(data1,countrycodes,by=c("country"="Country")) %>% 
  select(-7)
data3<-left_join(data2,data0,by=c("country","code","year","gdp.per.cap")) %>% 
  filter(between(year,1990,2020))

#CO2 emissions from https://zenodo.org/record/5569235#.Yrq2pnZBy5d
data12 <- read.csv("co2emissions.csv", header = TRUE, stringsAsFactors=FALSE, na.strings = c("", "NA")) %>% 
  rename(country=1,code=2,year=3,total.emissions=4) %>% 
  select(1,2,3,4) %>% 
  group_by(country) %>% 
  filter(country=="Global")

#Life expectancy from https://ourworldindata.org/life-expectancy
lifeexp<-read.csv("life-expectancy.csv", header = TRUE, stringsAsFactors=FALSE, na.strings = c("", "NA")) %>% 
  rename(country=Entity,code=Code,year=Year,life.exp=Life.expectancy)

#add continent info
datacontinent <- data3 %>%
  filter(continent != "is.NA") %>% 
  select(1,2,9)
data4<-left_join(data3,datacontinent,by=c("country","code")) %>% 
  select(-9) %>% 
  rename(continent=continent.y)
data5<-left_join(data4,lifeexp,by=c("country","code","year"))
data2019 <- data5 %>% 
  filter(year==2019)

#plot emissions cuts needed to stay within global carbon budget
x <-ggplot(data12, aes(x=year, y=total.emissions/1000)) +
  geom_line(show.legend=FALSE,color="blue") +
  scale_x_continuous(limits=c(1850,2050)) +
  labs(title="Steep pollution cuts are needed to keep warming to 1.5 C", x = NULL, y=NULL, subtitle = "CO2 Emissions (GtCO2)", x = "GDP per capita", y = "CO2 Emissions per capita",caption="Source: Global Carbon Project, IPCC, own calculations") +
  geom_segment(aes(x=2020,y=34.807,xend=2044,yend=0),color="red",linetype="dashed")
x 
ggsave("graph5.svg",plot=x, width=10, height=8)

#set minor breaks for logarithmic axes for charts with gdp
mb <- unique(as.numeric(1:10 %o% 10 ^ (0:5)))

#plot life expectancy and gdp per cap
o <- ggplot(data2019, aes(x=gdp.per.cap,y=life.exp,size=population))+
  geom_point(alpha=0.7, aes(color=continent), show.legend=FALSE) +
  scale_x_log10(minor_breaks=mb) +
  scale_y_continuous(limits=c(50,90)) +
  labs(title= "People in richer countries live longer...", y="Life Expectancy",x = "GDP per capita",caption="Source: Our World in Data")+
  scale_size(range = c(.1, 18), name="Population",guide='none')
o
ggsave("graph1.svg",plot=o, width=10, height=8)

#plot co2 per cap and gdp per cap
r<-ggplot(data2019, aes(x=gdp.per.cap, y=emissions.per.cap, size = population)) +
  geom_point(show.legend=FALSE, alpha=0.7, aes(color=continent)) +
  scale_color_viridis_d()+
  scale_size(range = c(2, 12)) +
  scale_x_log10(minor_breaks=mb) +
  labs(title="...but pollute the atmosphere with more CO2", x = "GDP per capita", y = "CO2 Emissions per capita",caption="Source: Our World in Data, Global Carbon Project")
r
ggsave("graph2.svg",plot=r, width=10, height=8)

#calculate emissions intensity for decoupling
data6<-data5 %>% 
  mutate(emissions.intensity = emissions.per.cap/gdp.per.cap) %>% 
  group_by(country) %>% 
  filter(country %in% c("France","Germany", "United Kingdom", "United States","Bangladesh","Brazil","China","Egypt","Nigeria","Indonesia","Italy","Japan","Mexico","Nigeria","Pakistan","Philippines","Russia","Thailand","Turkey","Vietnam","Saudi Arabia"))

#calculate percentage change since 1990 for gdp and emissions

#first find the baseline and add it to the main data
data1990 <- data6 %>%
  group_by(country) %>% 
  filter(year == 1990) %>% 
  select(1,2,4,6) %>% 
  rename(gdp.1990 = gdp.per.cap) %>% 
  rename(emissions.1990 = emissions.per.cap)
data7 <- left_join(data6,data1990,by=c("country","code"))

#calculate that percentage increase from the baseline
data8 <- data7 %>% 
  group_by(country) %>% 
  mutate(pc.increase.gdp = (gdp.per.cap - gdp.1990)/lag(gdp.per.cap)*100) %>%
  mutate(pc.increase.emissions = (emissions.per.cap - emissions.1990)/lag(emissions.per.cap)*100)
data8[is.na(data8)]<-0

#calculate the extent of decoupling in 2020
data9 <- data8 %>% 
  filter(year==2020) %>% 
  mutate(diff2020 = pc.increase.gdp-pc.increase.emissions) %>% 
  select(1,2,16)
data10<-left_join(data8,data9,by=c("country","code")) %>% 
  arrange(desc(diff2020)) %>% 
  mutate(diff2020=as.factor(diff2020)) %>% 
  group_by(country)

#plot decoupling in big economies
v<-ggplot(data10, aes(x=year,group=country)) +
  geom_line(aes(y=pc.increase.gdp),color="red")+
  geom_line(aes(y=pc.increase.emissions),color="blue") +
  labs(c("GDP","CO2"))+
  xlab(NULL)+
  ylab(NULL)+
  facet_wrap(~country, ncol=4)+ 
  labs(title = "Only some countries have decoupled <b style='color:red'>GDP</b> from <b style='color:blue'>CO2</b>", 
       subtitle = "Percentage change in <b style='color:red'>GDP</b> and  <b style='color:blue'>CO2 emissions</b> since 1990", x = "GDP per capita", y = "CO2 Emissions per capita",caption="Source: Our World in Data, Global Carbon Project") +
  theme(plot.title = element_markdown(lineheight = 1.1),
        plot.subtitle = element_markdown(lineheight = 1.1))
ggsave("graph4.svg",plot=v, width=10, height=8)

#plot decoupling in germany
data11<-data8 %>% 
  filter(country=="Germany")
w<-ggplot(data11, aes(x=year)) +
  geom_line(aes(y=pc.increase.gdp),color="red")+
  geom_line(aes(y=pc.increase.emissions),color="blue") +
  labs(c("GDP","CO2"))+
  xlab(NULL)+
  ylab(NULL) + 
  labs(title = "Germany is growing <b style='color:red'>richer</b> and <b style='color:blue'>cleaner</b>", 
       subtitle = "Percentage change in <b style='color:red'>GDP</b> and  <b style='color:blue'>C02 emissions</b> since 1990", x = "GDP per capita", y = "CO2 Emissions per capita",caption="Source: Our World in Data, Global Carbon Project") +
  theme(plot.title = element_markdown(lineheight = 1.1),
        plot.subtitle = element_markdown(lineheight = 1.1))
ggsave("graph3.svg",plot=w, width=10, height=8)