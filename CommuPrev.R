rm(list = ls()) 
graphics.off() 
setwd("C:/Rdirectory/CUTSEC/Data")
mydata <- read.csv("../Data/combined.csv", sep= ';', head=T)

#install.packages("plotly")
#install.packages("shiny")
library(plotly)
library(dplyr)
Trav<- subset(mydata, mydata$TraitName  =='Acquisition rate of returning traveller ')
CKprev<- subset(mydata, mydata$TraitName  =='PoultryESBL')
Comprev<- subset(mydata, mydata$TraitName=='CommunityESBL')
Decol<- subset(mydata,mydata$TraitName == 'Persistence rate of returning traveller') 
#put community in x-axis 
Acquisition <- plot_ly(human, x = ~Time, y = ~TraitValue, text = ~Note, type = 'scatter', mode = 'markers', size = ~SamplesizeSqrt, color = ~Continent , colors = 'Paired',
             marker = list(opacity = 0.5, sizemode = 'diameter')) %>%
  layout(title = 'Traveller acquired ESBL',
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
CK<-unique(CKprev$Continent)
Travel <-unique(Trav$Continent)
COMM <- unique(Comprev$Continent)

CK[CK %in% Travel]
CK[!(CK %in% Travel)]
Travel[!(Travel %in% CK)]
Travel[!(Travel %in% COMM)]
COMM[!(COMM %in% Travel)]
