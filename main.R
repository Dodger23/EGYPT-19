library(scales)
library(kableExtra)
library(webshot)
library(xtable)
library(gridExtra)
library(ggplot2)
library(tidyverse)
library(hrbrthemes)
library(viridis)
library(apcluster)
library(reshape2)
library(minpack.lm)
library(plotly)

options(scipen = 999)


getData <- function(item )
{
  library(dplyr)
  link = paste("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_" ,
               item , "_global.csv" , sep ="" )
  data = read.csv(url(link) , sep =",")
  
  #dropping State , Lat , Long columns 
  drop = c(1 ,3 , 4)
  data = select (data,-drop)
  
  
  for(i in 2:ncol(data))
  {
    names(data)[i] = sub('X', '',names(data)[i])
    names(data)[i] = as.character(as.Date(names(data)[i] , "%m.%d.%y"))
  }
  
  # Grouping By Country instead of States
  names(data)[1] = "Country"
  data = as.data.frame(data %>%  group_by(Country) %>%  summarise_all(sum) )
  
  #Filtering Egypt data only 
  data = data %>% filter(Country == "Egypt")
  
  data 
  
}



confirmed_cases = getData("confirmed")
death_cases = getData("deaths")
recovered_cases = getData("recovered")

write_csv(confirmed_cases , "Data/Egypt_confirmed_cases.csv")
write_csv(death_cases , "Data/Egypt_death_cases.csv")
write_csv(recovered_cases , "Data/Egypt_recovered_cases.csv")




