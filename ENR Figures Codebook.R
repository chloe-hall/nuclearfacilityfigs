
knitr::opts_chunk$set(echo = TRUE)
setwd("~/Dropbox/Slowing Nuclear Proliferation/Data Analysis/ENR")

#Load necessary packages
library(readxl)
library(tidyverse)
library(ggplot2)
library(dbplyr)

#Load the data
enr <- read_excel("ENR facility spreadsheet april.xlsx")

#select to relevant columns
enr<-enr %>% 
  select(country_name, ccode, facility_name, construction_start, construction_start_lower_bound, construction_end_upper_bound, construction_end, construction_end_lower_bound, construction_start_upper_bound, enr_type)


#list of countries in dataset
country<-unique(enr$country_name)

final <- data.frame(matrix(ncol = 6, nrow = 0))
colnames(final)<- c("country_name", "ccode", "facility_name", "start", "end")

for (row in 1:nrow(enr)) {
  if (is.na(enr[row, 4]) || enr[row, 4] < 0||enr[row, 4] > 3000) {
    start = enr$construction_start_lower_bound[row]
  } 
  else{
    start = enr$construction_start[row]
  }
  
  if (is.na(enr[row, 7] )|| enr[row, 7] < 0||enr[row, 7] > 3000) {
    end= enr$construction_end_lower_bound[row]
  }  else{
    end = enr$construction_end[row]
  }
  
  
  if(!is.na(start)&!is.na(end)){
    final[nrow(final) + 1, ] <-
      c(enr[row, 1], enr[row, 2], enr[row, 3], start, end, enr[row,10])
  }
}

df<- data.frame(matrix(ncol = 5, nrow = 0))
colnames(df)<- c("country_name", "ccode", "start", "years_to_build", "enr_type")

#Creating Variable for build time
for (row in 1:nrow(final)) {
  years_to_build = final$end[row]-final$start[row]
  
  if(years_to_build>=0){ #Removing the weird negative range values 
    df[nrow(df) + 1, ] <-
      c(final[row, 1], final[row, 2], final[row, 4], years_to_build, final[row,6])
  }
}

str(df)
df$start<-as.Date(as.character(df$start), format = "%Y")
df$years_to_build<-as.numeric(df$years_to_build)
str(df)

p5<-df %>% 
  filter(country_name=="China"|country_name=="France"|country_name=="Russia"|country_name=="United Kingdom"|country_name=="United States")

ggplot(p5, aes(x=start, y=years_to_build, color=country_name)) +
  geom_point()+ 
  geom_smooth(method = loess, se = T, color = "black")+
  ggtitle("P5 Countries Time to Build")

ggplot(p5, aes(x=start, y=years_to_build, color=country_name)) +
  annotate("text", x=8000,y=35,label=(paste0("OLSslope==",coef(lm(p5$years_to_build~p5$start))[2])),parse=TRUE)+
  geom_point()+ 
  geom_smooth(method = lm, se = T, color = "black")+
  ggtitle("P5 Countries Time to Build")

#Calculating the outliers
Q <- quantile(df$years_to_build, probs=c(.25, .75), na.rm = FALSE)

iqr <- IQR(df$years_to_build)

up <-  Q[2]+1.5*iqr # Upper Range 
up
low<- Q[1]-1.5*iqr # Lower Range
low


outliers<- subset(df, df$years_to_build < (Q[1] - 1.5*iqr) | df$years_to_build > (Q[2]+1.5*iqr))
outliers

nonoutliers<-subset(df, df$years_to_build > (Q[1] - 1.5*iqr) & df$years_to_build < (Q[2]+1.5*iqr))

p5norm<-nonoutliers %>% 
  filter(country_name=="China"|country_name=="France"|country_name=="Russia"|country_name=="United Kingdom"|country_name=="United States")

ggplot(p5norm, aes(x=start, y=years_to_build, color=country_name)) +
  geom_point()+ 
  geom_smooth(method = loess, se = T, color = "black")+
  ggtitle("P5 Countries Time to Build")+
  xlab("Year Construction Started")+
  ylab("Number of Years to Build")+
  theme(plot.title = element_text(hjust = 0.5))


#OECD Figures
oecd<-df %>% 
  filter(country_name=="Australia"|country_name=="Belgium"|country_name=="Canada"|country_name=="Czech Republic"|country_name=="France"|country_name=="Germany"|country_name=="Israel"|country_name=="Italy"|country_name=="Japan"|country_name=="Netherlands"|country_name=="Norway"|country_name=="South Korea"|country_name=="Spain"|country_name=="Sweden"|country_name=="United Kingdom"|country_name=="United States")

#The lm line coef
coef(lm(oced$years_to_build~oced$start))[2]

ggplot(oecd, aes(x=start, y=years_to_build, color=country_name)) +
  geom_point()+ 
  geom_smooth(method = lm, se = T, color = "black")+
  ggtitle("OCED Countries Time to Build")



#Non OECD
nonoecd<-df %>% 
  filter(country_name!="Australia"&country_name!="Belgium"&country_name!="Canada"&country_name!="Czech Republic"&country_name!="France"&country_name!="Germany"&country_name!="Israel"&country_name!="Italy"&country_name!="Japan"&country_name!="Netherlands"&country_name!="Norway"&country_name!="South Korea"&country_name!="Spain"&country_name!="Sweden"&country_name!="United Kingdom"&country_name!="United States")

#The lm line coef
coef(lm(nonoecd$years_to_build~nonoecd$start))[2]

ggplot(nonoecd, aes(x=start, y=years_to_build, color=country_name)) +
  geom_point()+ 
  geom_smooth(method = lm, se = T, color = "black")+
  ggtitle("Non-OCED Countries Time to Build")
