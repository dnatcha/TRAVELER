#################################################
#      Visualise raw data to observe trends     #
#################################################

# set working directory
setwd('~/Documents/nBox/git_projects/TRAVELER-main/')

# load libraries
library(WDI)
library(plotly); library(ggplot2)
library(dplyr)
library(stringr)

theme_set(theme_minimal())

# load raw data 
dat.raw = read.csv('combined.csv', sep = ',')

# get GDP from world bank
GDP <- WDI(
  country = "all",
  indicator = "NY.GDP.PCAP.KD",
  start = 2019,
  end = 2019,
  extra = FALSE,
  cache = NULL,
  latest = NULL,
  language = "en"
)

# add gdp to data 
dat <- merge(dat.raw, GDP, by.x = "Location_cod", by.y = "iso2c", all.x = T)
colnames(dat)[which(colnames(dat) == "NY.GDP.PCAP.KD")] = 'gdp'

## types of data available 
unique(dat$TraitName)

#===================================#
#       Plot trends with time       #
#===================================#

# Community prevalence at destination countries - grouped by gdp 
data.human.destination = dat[which(dat$TraitName == 'CommunityESBL'),]
ggplot(data.human.destination, aes(x = Time, y = TraitValue, color = gdp)) + 
  geom_point(aes(size = SamplesizeSqrt)) + 
  labs(y = 'Proportion of MDRO carriers', x = '') + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), 
        legend.position = 'bottom') 

# Acquisition rate of returning traveller
data.human <- subset(dat, TraitName =='Acquisition rate of returning traveller ')


# get 
Trav <- subset(dat, TraitName =='Acquisition rate of returning traveller ')
CKprev <- subset(dat, TraitName =='PoultryESBL')
Comprev <- subset(dat, TraitName == 'CommunityESBL')
Decol <- subset(dat, TraitName == 'Persistence rate of returning traveller') 

#put community in x-axis 
plot_ly(Trav, x = ~ NY.GDP.PCAP.KD, y = ~ TraitValue, text = ~ Location_sov, 
                       type = 'scatter', mode = 'markers', size = ~ SamplesizeSqrt, 
                       color = ~ Location_cod , colors = 'Paired',
                       marker = list(opacity = 0.5, sizemode = 'diameter')) %>%
  layout(title = 'Traveller acquired ESBL',
         xaxis = list(showgrid = FALSE),
         yaxis = list(showgrid = FALSE),
         showlegend = T )

plot_ly(CKprev, x = ~ Location_sov, y = ~ TraitValue, text = ~ Note, 
                   type = 'scatter', mode = 'markers', size = ~ Samplesize, color = ~ Time , colors = 'Paired',
                   marker = list(opacity = 0.5, sizemode = 'diameter')) %>%
  layout(title = 'ESBL prevalence in chicken',
         xaxis = list(showgrid = FALSE),
         yaxis = list(showgrid = FALSE),
         showlegend = T)

PrevaCK.gdp <- plot_ly(CKprev, x = ~ NY.GDP.PCAP.KD, y = ~ TraitValue, text = ~ Note, 
                   type = 'scatter', mode = 'markers', size = ~ Samplesize, color = ~ Location_cod , colors = 'Paired',
                   marker = list(opacity = 0.5, sizemode = 'diameter')) %>%
  layout(title = 'ESBL prevalence in chicken',
         xaxis = list(showgrid = FALSE),
         yaxis = list(showgrid = FALSE),
         showlegend = T)

PrevCom <- plot_ly(Comprev, x = ~ Location_sov, y = ~ TraitValue, text = ~ Note, 
                   type = 'scatter', mode = 'markers', size = ~ Samplesize, color = ~ Time , colors = 'Paired',
                   marker = list(opacity = 0.5, sizemode = 'diameter')) %>%
  layout(title = 'ESBL prevalence in community',
         xaxis = list(showgrid = FALSE),
         yaxis = list(showgrid = FALSE),
         showlegend = T)

PrevCom.gdp <- plot_ly(Comprev, x = ~ NY.GDP.PCAP.KD, y = ~ TraitValue, text = ~ Note, 
                   type = 'scatter', mode = 'markers', size = ~ Samplesize, color = ~ Location_cod, colors = 'Paired',
                   marker = list(opacity = 0.5, sizemode = 'diameter')) %>%
  layout(title = 'ESBL prevalence in community',
         xaxis = list(showgrid = FALSE),
         yaxis = list(showgrid = FALSE),
         showlegend = T)



#### prevalence in community over time 
# humans 
##### community 
plot_ly(Comprev, x = ~ Time, y = ~ TraitValue, text = ~ Note, 
        type = 'scatter', mode = 'markers', size = ~ Samplesize, color = ~ Location_cod, 
        colors = 'Paired', marker = list(opacity = 0.5, sizemode = 'diameter')) %>%
  layout(title = 'ESBL prevalence in community',
         xaxis = list(showgrid = FALSE),
         yaxis = list(showgrid = FALSE),
         showlegend = T)

plot_ly(Comprev, x = ~ Time, y = ~ TraitValue, text = ~ Note, 
        type = 'scatter', mode = 'markers', size = ~ Samplesize, color = ~ NY.GDP.PCAP.KD, 
        colors = 'Paired', marker = list(opacity = 0.5, sizemode = 'diameter')) %>%
  layout(title = 'ESBL prevalence in community',
         xaxis = list(showgrid = FALSE),
         yaxis = list(showgrid = FALSE),
         showlegend = T)

##### returning traveller 
plot_ly(Trav, x = ~ Time, y = ~ TraitValue, text = ~ Note, 
        type = 'scatter', mode = 'markers', size = ~ Samplesize, color = ~ Location_cod, 
        colors = 'Paired', marker = list(opacity = 0.5, sizemode = 'diameter')) %>%
  layout(title = 'Traveller acquired ESBL',
         xaxis = list(showgrid = FALSE),
         yaxis = list(showgrid = FALSE),
         showlegend = T)

plot_ly(Trav, x = ~ Time, y = ~ TraitValue, text = ~ Note, 
        type = 'scatter', mode = 'markers', size = ~ Samplesize, color = ~ NY.GDP.PCAP.KD, 
        colors = 'Paired', marker = list(opacity = 0.5, sizemode = 'diameter')) %>%
  layout(title = 'Traveller acquired ESBL',
         xaxis = list(showgrid = FALSE),
         yaxis = list(showgrid = FALSE),
         showlegend = T)


# chicken 
plot_ly(CKprev, x = ~ Time, y = ~ TraitValue, text = ~ Note, 
        type = 'scatter', mode = 'markers', size = ~ Samplesize, color = ~ Location_cod, 
        colors = 'Paired', marker = list(opacity = 0.5, sizemode = 'diameter')) %>%
  layout(title = 'ESBL prevalence in community',
         xaxis = list(showgrid = FALSE),
         yaxis = list(showgrid = FALSE),
         showlegend = T)

plot_ly(CKprev, x = ~ Time, y = ~ TraitValue, text = ~ Note, 
        type = 'scatter', mode = 'markers', size = ~ Samplesize, color = ~ NY.GDP.PCAP.KD, 
        colors = 'Paired', marker = list(opacity = 0.5, sizemode = 'diameter')) %>%
  layout(title = 'ESBL prevalence in community',
         xaxis = list(showgrid = FALSE),
         yaxis = list(showgrid = FALSE),
         showlegend = T)





#compare locations#
CK<- unique(CKprev$Continent)
Travel <- unique(Trav$Continent)
COMM <- unique(Comprev$Continent)

CK[CK %in% Travel]
CK[!(CK %in% Travel)]
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
