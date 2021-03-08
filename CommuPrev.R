rm(list = ls()) 
graphics.off() 
setwd("C:/Rdirectory/CUTSEC/Data")
library(WDI)
library(plotly)
library(dplyr)
library(stringr)
mydata <- read.csv("../Data/combined.csv", sep= ';', head=T)
df<-mydata[,c(1,3,5,6,7,9,10,11,13)]

GDP<-WDI(
  country = "all",
  indicator = "NY.GDP.PCAP.KD",
  start = 2019,
  end = 2019,
  extra = FALSE,
  cache = NULL,
  latest = NULL,
  language = "en"
)

df$Location_cod<- str_trim(df$Location_cod) 
GDP_cut<-GDP %>%
  filter(iso2c %in% df$Location_cod)
GDP_cut<-GDP_cut[,-c(2,4)]
df<-merge(df,GDP_cut, by.x="Location_cod", by.y="iso2c", all.x = T)
colnames(df)[10]<-"GDPP_2019"
hist(df$GDPP_2019, breaks=40) 

#group GDP in range of 2000?
df$Group <- ifelse(GDPP_2019 <= 2000 , Dates + 1, Dates)
df$Group <- ifelse(GDPP_2019 =< 3000 , 1,
       ifelse(GDPP_2019 < 3000 & GDPP_2019 =< 6000, 2, <no>), 
       ifelse(<condition>, <yes>, <no>)
)

GDP[GDP$iso2c== "CH",]   #indices

str(new_Dates)
Trav<- subset(df, df$TraitName  =='Acquisition rate of returning traveller ')
CKprev<- subset(df, df$TraitName  =='PoultryESBL')
Comprev<- subset(df, df$TraitName=='CommunityESBL')
Decol<- subset(df,df$TraitName == 'Persistence rate of returning traveller') 


#put community in x-axis 
Acquisition <- plot_ly(Trav, x = ~GDPP_2019, y = ~TraitValue, text = ~Location_sov, type = 'scatter', mode = 'markers', size = ~SamplesizeSqrt, color = ~Location_cod , colors = 'Paired',
             marker = list(opacity = 0.5, sizemode = 'diameter')) %>%
  layout(title = 'Traveller acquired ESBL',
         xaxis = list(showgrid = FALSE),
         yaxis = list(showgrid = FALSE),
         showlegend = T )

# community prev  (same GDP)
PrevCom <- plot_ly(Comprev, x = ~GDPP_2019, y = ~TraitValue, text = ~Location_sov, type = 'scatter', mode = 'markers', size = ~SamplesizeSqrt, color = ~Location_cod , colors = 'Paired',
                       marker = list(opacity = 0.5, sizemode = 'diameter')) %>%
  layout(title = 'Community ESBL',
         xaxis = list(showgrid = FALSE),
         yaxis = list(showgrid = FALSE),
         showlegend = T )

## travel ~ community #############
PrevCom <- plot_ly(Comprev, x = ~GDPP_2019, y = ~TraitValue, text = ~Location_sov, type = 'scatter', mode = 'markers', size = ~SamplesizeSqrt, color = ~Location_cod , colors = 'Paired',
                   marker = list(opacity = 0.5, sizemode = 'diameter')) %>%
  layout(title = 'Community ESBL',
         xaxis = list(showgrid = FALSE),
         yaxis = list(showgrid = FALSE),
         showlegend = T )


PrevaCK <- plot_ly(CKprev, x = ~Location, y = ~TraitValue, text = ~Note, type = 'scatter', mode = 'markers', size = ~Samplesize, color = ~Time , colors = 'Paired',
                       marker = list(opacity = 0.5, sizemode = 'diameter')) %>%
  layout(title = 'ESBL prevalence in chicken',
         xaxis = list(showgrid = FALSE),
         yaxis = list(showgrid = FALSE),
         showlegend = T)


PrevCom <- plot_ly(Comprev, x = ~Location, y = ~TraitValue, text = ~Note, type = 'scatter', mode = 'markers', size = ~Samplesize, color = ~Time , colors = 'Paired',
                   marker = list(opacity = 0.5, sizemode = 'diameter')) %>%
  layout(title = 'ESBL prevalence in community',
         xaxis = list(showgrid = FALSE),
         yaxis = list(showgrid = FALSE),
         showlegend = T)

dev.off()
print(Acquisition)
print(PrevaCK)
print(PrevCom)

#compare locations#
CK<-unique(CKprev$Location_sov)
Travel <-unique(Trav$Location_sov)
COMM <- unique(Comprev$Location_sov)

CK[CK %in% Travel] #list of countries availavle in both chicken and travelers
CK[!(CK %in% Travel)]  #list of countries available in chicken but not in returning travelers
Travel[!(Travel %in% CK)]
Travel[!(Travel %in% COMM)]
COMM[!(COMM %in% Travel)]

WDIsearch('gdp.*capita.*constant')
WDIsearch('gdp')
GDP<-WDI(
  country = "all",
  indicator = "NY.GDP.PCAP.KD",
  start = 2010,
  end = 2019,
  extra = FALSE,
  cache = NULL,
  latest = NULL,
  language = "en"
)


a="abc "
a=" abc "
str_trim(a) 
str_trim(a, 'right') 
toupper( "abc" )

chick<- subset(mydata, mydata$TraitName == "PoultryESBL")
