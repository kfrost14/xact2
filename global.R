library(shiny)
#library(shinyIncubator)
library(rCharts)
library(openair)
library(ggplot2)
library(maps)
#library(stringr)
#library(Hmisc)
library(googleVis)
library(gridExtra)
library(rgdal)
library(RColorBrewer)


setwd("R:/Development/Kali/metalsapp")

rfc<-read.csv("R:/Development/Kali/metalsapp/rfc.csv",colClasses=c("character","numeric","numeric"))
xact_param<-c("Potassium", "Calcium", "Titanium", "Chromium", "Manganese", 
"Iron", "Cobalt", "Nickel", "Copper", "Zinc", "Arsenic", "Selenium", 
"Bromine", "Rubidium", "Strontium", "Molybdenm", "Cadmium", "Antimony", 
"Barium", "Mercury", "Thallium", "Lead", "Thorium")
wswd_names<-read.csv("wswd_county_names.csv")


site<-read.csv("site_data.csv",header=TRUE,colClasses=c(rep("factor",15),"numeric","numeric"))


xact<-import("gary_xact_met.csv",date="date",date.format="%m/%d/%Y %H:%M")
colnames(xact)<-c("date", "AT.C",	"BP.mmHg",	"ALARM",	"Potassium", "Calcium", "Titanium", "Chromium", "Manganese", 
                  "Iron", "Cobalt", "Nickel", "Copper", "Zinc", "Arsenic", "Selenium", 
                  "Bromine", "Rubidium", "Strontium", "Molybdenm", "Cadmium", "Antimony", 
                  "Barium", "Mercury", "Thallium", "Lead", "Thorium",	"YEAR",	"MONTH",	"DAY",	"HOUR",	"ws",	"wd",	"peak wind gust",	"std dev hz wd",	"vert wd	temp")


emissions<-as.data.frame(local(get(load("emissions2.RData"))),stringsAsFactors=FALSE)
emissions$YEAR<-as.factor(format(as.POSIXct(as.character(emissions$YEAR),tz="UTC",format="%Y"),"%Y"))
emissions$LAT_LONG<-paste(emissions$LAT,emissions$LONG,sep="_")
names(emissions)[29]<-"EmissionsTPY"

data <- as.data.frame(local(get(load("metals_long_form_data2.RData"))),row.names=NULL)
x<-as.numeric(data$Site.Num)
x<- sprintf("%04d",x)
data$Site.Num<-as.factor(x)
completeVec <- complete.cases(data[, 18])
data<-data[completeVec, ]

wswd<-as.data.frame(local(get(load("pm25_wswd_site.Rdata"))))

hap<-c("Lead","Beryllium","Manganese","Chromium","Cadmium","Mercury","Nickel","Arsenic","Cobalt")
ps<-paste(hap," Compounds",sep="")
haps<-append(hap,ps)


for(i in 1:4){wswd[,i]<-as.factor(wswd[,i])}
for(i in 5:6){wswd[,i]<-as.factor(wswd[,i])}


