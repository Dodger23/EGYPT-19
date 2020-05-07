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
library(lubridate)

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

  # Grouping By Country instead of States
  names(data)[1] = "Country"
  data = as.data.frame(data %>%  group_by(Country) %>%  summarise_all(sum) )
  
  #Filtering Egypt data only 
  data = data %>% filter(Country == "Egypt")
  
  write_data(data , item)
  
  names(data)[2:ncol(data)] = sub('X' , '' , names(data)[2:ncol(data)])
  names(data)[2:ncol(data)] = as.Date(names(data)[2:ncol(data)] , "%m.%d.%y")

  data 
  
}

write_data = function(data , name)
{
  names(data)[2:ncol(data)] = sub('X' , '' , names(data)[2:ncol(data)])
  names(data)[2:ncol(data)] = as.character(as.Date(names(data)[2:ncol(data)] , "%m.%d.%y") )
  write_csv(data , paste0("Data/" , "Egypt_" , name , "_cases.csv" ))
}



model = function(data , countryName , date , type)
{
  
  #This is a function to make an exponential prediction growth about a country 
  #Formula =  alpha *exp(beta*t) + theta 
  
  
  #Saving Dates in a new dataframe as dates instead of number of days since 1970-1-1 as column 1
  # Saving Cases of the wanted country as column 2 
  df_t = data.frame('Date' = as.numeric(names(data)[2:ncol(data)] ) - 18283  , 
                    'Cases' = as.numeric(as.vector(data[1,2:ncol(data)]) ) 
  )
  
  #Filter the days which number of cases was less than 1 to better fit the model 
  df_t = df_t %>% filter(Cases > 1 )
  
  # Assigning theta to half of the minimum value in Dates
  c.0 =   min(df_t$Date)* 0.5
  
  #Fitting a model to find the optimum value of theta 
  model.0 <- lm(log(Cases - c.0) ~ Date, data=df_t)
  
  # Finding optimum of alpha, beta and theta 
  start <- list(a=exp(coef(model.0)[1]), b=coef(model.0)[2], c=c.0)
  model = nls(formula = Cases ~ a * exp(b * Date) + c , data = df_t,  start = start)
  
  # Storing alpha, beta and theta 
  t = coef(model)
  
  
  # Storing dates and original cases 
  dt1 = data.frame(Date = as.Date(as.numeric(df_t$Date) , origin = '2020-1-22') , 
                   Cases = df_t$Cases
  )
  
  # adding new dates untill the wanted date 
  days =as.Date(date , format = "%d-%m")-  as.Date("22-1" ,format = "%d-%m") 
  days = days - 1 
  x = df_t$Date
  for( i  in max(x):days)
  {
    x = c(x , max(x) +1)
  }
  
  
  # growth curve by Formula ( alpha * exp(beta* Date) + theta )
  p = t["a"] * (exp(t["b"] * x)) + t["c"]
  
  
  #Filter negative values out 
  p[p<0] = 0
  
  
  # Storing dates and cases of the prediction curve  
  dt2 = data.frame(Date = as.Date(x , origin = '2020-1-22') ,
                   Cases = round(p)  )
  
  #plotting the two curves 
  if(! file.exists(paste0("images/" , date) ))
    dir.create(paste0("images/" , date))
  
  png(filename = paste0("images/" ,date, "/exponintial_" ,type, ".png")  , width = 1366 , height = 768 , units = "px" )
  pl = ggplot(NULL) +
    geom_line(data = dt2  ,aes(  x = Date , y = Cases , color = "Predicted Cases") , size = 1) +
    geom_line(data = dt1 , aes( x = Date , y = Cases  , color = "confirmed Cases") , size = 1) +
    theme_ft_rc() +
    labs(x = "Date", y = "Cases")+
    ggtitle(paste0("Prediction of COVID19 Cases of EGYPT on " , date))
  print(pl)
  dev.off()
  
}




confirmed_cases = getData("confirmed")
death_cases = getData("deaths")
recovered_cases = getData("recovered")

#data = read.csv("Data/Egypt_confirmed_cases.csv")
#names(data)[2:ncol(data)] = sub('X' , '' , names(data)[2:ncol(data)])
#names(data)[2:ncol(data)] = as.Date(names(data)[2:ncol(data)] , "%Y.%m.%d") 

model(confirmed_cases , "Egypt" , "25-5" , "confirmed_cases")
model(death_cases , "Egypt" , "25-5" , "deaths_cases")
model(death_cases , "Egypt" , "25-5" , "recovered_cases")







