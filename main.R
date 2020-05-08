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
library(dplyr)

options(scipen = 999)


getData <- function(item)
{

  link = paste(
    "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_" ,
    item ,
    "_global.csv" ,
    sep = ""
  )
  data = read.csv(url(link) , sep = ",")
  
  #dropping State , Lat , Long columns
  drop = c(1 , 3 , 4)
  data = select (data, -drop)
  
  # Grouping By Country instead of States
  names(data)[1] = "Country"
  data = as.data.frame(data %>%  group_by(Country) %>%  summarise_all(sum))
  
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
  names(data)[2:ncol(data)] = as.character(as.Date(names(data)[2:ncol(data)] , "%m.%d.%y"))
  write_csv(data , paste0("Data/" , "Egypt_" , name , "_cases.csv"))
}

cleanData = function(data)
{
  df = data.frame(
    Date = as.Date(as.numeric(names(data)[2:ncol(data)]), origin = '1970-1-1'),
    Confirmed_Cases = as.numeric(data[1 , 2:ncol(data)])
  )
  df
}


model = function(data , countryName , date , type)
{
  #This is a function to make an exponential prediction growth about a country
  #Formula =  alpha *exp(beta*t) + theta
  
  
  #Saving Dates in a new dataframe as dates instead of number of days since 1970-1-1 as column 1
  # Saving Cases of the wanted country as column 2
  df_t = data.frame('Date' = as.numeric(names(data)[2:ncol(data)]) - 18283  ,
                    'Cases' = as.numeric(as.vector(data[1, 2:ncol(data)])))
  
  #Filter the days which number of cases was less than 1 to better fit the model
  df_t = df_t %>% filter(Cases > 1)
  
  # Assigning theta to half of the minimum value in Dates
  c.0 =   min(df_t$Date) * 0.5
  
  #Fitting a model to find the optimum value of theta
  model.0 <- lm(log(Cases - c.0) ~ Date, data = df_t)
  
  # Finding optimum of alpha, beta and theta
  start <-
    list(a = exp(coef(model.0)[1]),
         b = coef(model.0)[2],
         c = c.0)
  model = nls(
    formula = Cases ~ a * exp(b * Date) + c ,
    data = df_t,
    start = start
  )
  
  # Storing alpha, beta and theta
  t = coef(model)
  
  
  # Storing dates and original cases
  dt1 = data.frame(Date = as.Date(as.numeric(df_t$Date) , origin = '2020-1-22') ,
                   Cases = df_t$Cases)
  
  # adding new dates untill the wanted date
  days = as.Date(date , format = "%d-%m-%Y") -  as.Date("22-1-2020" , format = "%d-%m-%Y")
  days = days - 1
  x = df_t$Date
  for (i  in max(x):days)
  {
    x = c(x , max(x) + 1)
  }
  
  
  # growth curve by Formula ( alpha * exp(beta* Date) + theta )
  p = t["a"] * (exp(t["b"] * x)) + t["c"]
  
  
  #Filter negative values out
  p[p < 0] = 0
  
  
  # Storing dates and cases of the prediction curve
  dt2 = data.frame(Date = as.Date(x , origin = '2020-1-22') ,
                   Cases = round(p))
  
  #plotting the two curves
  if (!file.exists(paste0("images/" , date)))
    dir.create(paste0("images/" , date))
  
  png(
    filename = paste0("images/" , date, "/exponintial_" , type, ".png")  ,
    width = 1366 ,
    height = 768 ,
    units = "px"
  )
  pl = ggplot(NULL) +
    geom_line(data = dt2  ,
              aes(x = Date , y = Cases , color = "Predicted Cases") ,
              size = 1) +
    geom_line(data = dt1 ,
              aes(x = Date , y = Cases  , color = "confirmed Cases") ,
              size = 1) +
    theme_ft_rc() +
    labs(x = "Date", y = "Cases") +
    ggtitle(paste0("Prediction of COVID19 " , type ," of EGYPT on " , date))
  print(pl)
  dev.off()
  
}


sumulated_from_current = function (df , date , type)
{
  
  getRate = function(i , infected)
  {
    population = 98420000
    avg_infected_human_interActions_per_day = 7/i
    x = avg_infected_human_interActions_per_day  * (1 - infected / population)
    
    x
  }

  data = df
  data$New_Cases = c(rep(0, times = nrow(data)))
  data$Actual_Cases = data$Confirmed_Cases
  data$CR = c(rep(0, times = nrow(data)))
  
  x = data$Date
  y = data$Confirmed_Case
  
  days = as.Date(date, format = "%d-%m") - as.Date(df[nrow(df) , "Date"]  , format =
                                                        "%Y-%m-%d")
  for (i in 1:days)
  {
    x = c(x  , max(x) + 1)
    y = c(y , 0)
  }
  
  next_Days = data.frame(Date = x , Cases = y)
  
  for (i in nrow(data):(nrow(next_Days) - 1))
  {
    infection_rate = getRate(i , next_Days[i , "Cases"])
    next_Days[i + 1 , "Cases"] = (1 + infection_rate) *  next_Days[i , "Cases"]
  }
  
  dtt = next_Days[, c("Date" , "Cases")]
  dtt$New_Cases  = rep(0 , nrow(dtt))
  for (i in 2:nrow(dtt))
  {
    dtt[i , "New_Cases"] = dtt[i, "Cases"] - dtt[i - 1 , "Cases"]
  }
  
  
  if (!file.exists(paste0("images/" , date)))
    dir.create(paste0("images/" , date))
  
  png(
    filename = paste0("images/" , date ,"/", type, "_infection_rate", ".png")  ,
    width = 1366 ,
    height = 768 ,
    units = "px"
  )
  pl = ggplot(NULL) +
    geom_line(data  = dtt , aes(Date , New_Cases  , color = "New Cases")) +
    geom_line(data  = next_Days , aes(Date , round(Cases)  , color = "Predicted Cases")) +
    geom_line(data  = data , aes(Date , Confirmed_Cases  , color = "Confirmed Cases")) +
    theme_ft_rc()  +
    labs(title = paste0("The predicted ", type, " cases in Egypt on " , date) , y = "Cases" )
  #ggplotly(pl  , tooltip = c("x" , "y"))
  print(pl)
  dev.off()
  
}


simulated_from_patient0 = function (data , date , type)
{
  getRate = function(data , i , infected)
  {
    #Population of the wanted Country
    population = 98420000
    
    #Quarntine really  decreased the avg number of human interactions per day so it had to be generated from a smaller numbers after the date of quarntine
    #Start day of Quarntine
    start_of_Quarntine = 72
    
    
    #Generating random number represents the avg humans interactions per day in Egypt
    if (i >= start_of_Quarntine)
      avg_human_interActions_per_day = 7/ (i-23) 
    else
      avg_human_interActions_per_day = 2/  (i-23)
    
    
    # infection ratio formula
    
    infection_ratio = avg_human_interActions_per_day  * (1 - (infected / population) )

    #Return the answer
    infection_ratio
  }
  
  
  #Adding newe colum to store The Actual cases number for each day
  data$Actual_Cases = data$Confirmed_Cases
  
  #Adding new column to store Infection ration for each day
  data$IR = c(rep(0, times = nrow(data)))
  
  dtt = data
  days = as.Date(date, format = "%d-%m-%Y") - as.Date(dtt[nrow(dtt) , "Date"]  , format ="%Y-%m-%d")
  for (i in 1:days)
  {
    x  = data.frame(Date = dtt[nrow(dtt) , "Date"]+1  , Confirmed_Cases  = 0 , Actual_Cases = 0 , IR = 0 )
    dtt = rbind(dtt , x)
  }
  
  # Starting from the day which patient zero appeard on : 24 days after 22-1
  for (i in 24:nrow(dtt))
  {
    # Getting infection rate from the function and sending to it the number of cases of today
    infection_rate = getRate(dtt , i , dtt[i , "Actual_Cases"])
    # Storing Infection rate of today
    dtt[i, "IR"] = infection_rate
    
    #Calculating the number of cases will be tomorrow by the formula
    # N(d+1) = (N(d) + E * P) * N(d)
    # Taking ( N(d) ) as factor : N(d+1) = (1+E*P) * N(d)
    # N(d) = number of cases today
    # N(d+1) = number of cases tomorrow
    # E = avg humans interaction of this day
    # P = (Probability the person will get the infection from another one in an interaction
    dtt[i + 1 , "Actual_Cases"] = (1 + infection_rate) *  dtt[i , "Actual_Cases"]
    
  }
  
  
  if (!file.exists(paste0("images/" , date)))
    dir.create(paste0("images/" , date))
  
  png(
    filename = paste0("images/" , date ,"/", type, "_infection_rate_from_start", ".png")  ,
    width = 1366 ,
    height = 768 ,
    units = "px"
  )
  # Plotting Dates in the x axis and the two curves (Confirmed Cases , Actual Cases)
  pl = ggplot(NULL) +
    geom_line(data = data , aes(Date , Confirmed_Cases  , color = "Confirmed cases")) +
    geom_line(data = dtt , aes(Date , floor(Actual_Cases)  , color = "Simulated cases")) +
    theme_ft_rc()  +
    labs(title = paste0("The simulated ", type, " cases in Egypt on " , date) , y ="Cases")
  print(pl)
  dev.off()
  #ggplotly(pl  , tooltip = c("x" , "y"))
  
}




confirmed_cases = getData("confirmed")
death_cases = getData("deaths")
recovered_cases = getData("recovered")

model(confirmed_cases , "Egypt" , "25-5-2020" , "recovered_cases")
model(death_cases , "Egypt" , "25-5" , "deaths_cases")
model(recovered_cases , "Egypt" , "25-5" , "deaths_cases")


sumulated_from_current(cleanData(confirmed_cases) , "25-5-2020" , "confirmed" )
simulated_from_patient0(cleanData(confirmed_cases) , "25-5-2020" , "confirmed")

