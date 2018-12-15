library(readxl)
library(timelineS)
library(tidyverse)
library(xts)
library(ggplot2)
library(TTR)
library(esquisse)
library(lme4)

xl <- list.files("Data")

#Select xlsx_files
xl <- xl[grep('xlsx',xl)]

#Data pull was started on 19th Dec 
#On days with 2 observation, we will be taking the non-null values and avergae of 2 values if both values are present. 

#Analysising one day
day <- data.frame(airline=character(),
                  arrival=character(),
                  departure=character(),
                  "flight duration" = character(),
                  plane=character(),
                  "plane code"=character(),
                  stop=character(),
                  "ticket price"=character(),
                  timings=character(),
                  Date=as.Date(character()),
                  CurrDate= as.Date(character()),
                  RT = as.character(),
                  n_day = as.numeric(),
                  stringsAsFactors=TRUE)

for (i in 1:103){
  a <- as.data.frame(read_xlsx(paste0("Data/",xl[1]), sheet = i, n_max = 50))[,-1]
  if(nrow(a) > 1){
    a$n_day <- difftime(as.Date(a$Date, "%m/%d/%Y") , as.Date(a$CurrDate , "%m/%d/%Y" ) )
    day <- rbind(day,a)
  }
}

#Taking data from between 14 days to (90+14)days
#day <- day[which(day$n_day > 13),]
colnames(day)[c(4,6,8)] <- c("duration","plane_code","price")

#Finding cheapest fare in coming days 
as.data.frame(day %>%
  select(Date, price) %>%
  group_by(Date) %>%
  summarize(cheap_price = min(price)) )-> cheapest

cp_xts <- xts(x=cheapest$cheap_price, order.by= as.Date(cheapest$Date,"%m/%d/%Y"))
plot.ts(cp_xts)

#What we can see from the analysis of a logitudinal cut?
#If you are checking the tickets right now ( to book within next 10 days), there is not much choice left for you 
#Fare is higher at the recent dates. There is less reward of waiting if you are in period around 10-20 days
#Between 20-50 days, there is a situation of interest. Risk/Reward seem ambiguous
#After ~50 days the fare start to follow a normal trend. The offers are mostly introduced around 50 days

#Lets have a brief look of a middle day to see if they follow similar pattern ! 
day2 <- data.frame(airline=character(),
                  arrival=character(),
                  departure=character(),
                  "flight duration" = character(),
                  plane=character(),
                  "plane code"=character(),
                  stop=character(),
                  "ticket price"=character(),
                  timings=character(),
                  Date=as.Date(character()),
                  RT = as.character(),
                  n_day = as.numeric(),
                  stringsAsFactors=TRUE)

CurrDate <- as.Date('11242018',format = "%m%d%Y")
for (i in 1:103){
  a <- as.data.frame(read_xlsx(paste0("Data/",xl[10]), sheet = i, n_max = 50))[,-1]
  if(nrow(a) > 1){
    a$n_day <- difftime(as.Date(a$Date, "%m/%d/%Y") , CurrDate )
    day2 <- rbind(day2,a)
  }
}

colnames(day2)[c(4,6,8)] <- c("duration","plane_code","price")

#Finding cheapest fare in coming days 
as.data.frame(day2 %>%
                select(Date, price) %>%
                group_by(Date) %>%
                summarize(cheap_price = min(price)) )-> cheapest2

cp_xts2 <- xts(x=cheapest2$cheap_price, order.by= as.Date(cheapest2$Date,"%m/%d/%Y"))

#The graph from 10 days after also shows a similar trend which is good news

#Lets analyse the cross sectional data
#How is the fair trend for a day in the future ? 

#Data Cleaning and Export

clean <- function(dt, obj){
day2 <- data.frame(airline=character(),
                   arrival=character(),
                   departure=character(),
                   "flight duration" = character(),
                   plane=character(),
                   "plane code"=character(),
                   stop=character(),
                   "ticket price"=character(),
                   timings=character(),
                   Date=as.Date(character()),
                   RT = as.character(),
                   n_day = as.numeric(),
                   stringsAsFactors=TRUE)

CurrDate <- as.Date(dt,format = "%m%d%Y")
for (i in 1:103){
  a <- as.data.frame(read_xlsx("Data/06Dec_night.xlsx", sheet = i, n_max = 50))[,-1]
  a[!grepl("Curr",colnames(a))]
  if(nrow(a) > 1){
    a$n_day <- difftime(as.Date(a$Date, "%m/%d/%Y") , CurrDate )
    day2 <- rbind(day2,a)
  }
}

colnames(day2)[c(4,6,8)] <- c("duration","plane_code","price")

day2$CurrDate <- CurrDate

return(day2)
}


input <- as.data.frame(cbind( c( paste0("11",18:30,"2018") , "12012018"),  c(1,2,4,6,8,9,10,12,13,14,15,16,17,19)) , stringsAsFactors = FALSE)

day <- data.frame(airline=character(),
                   arrival=character(),
                   departure=character(),
                   "flight duration" = character(),
                   plane=character(),
                   "plane code"=character(),
                   stop=character(),
                   "ticket price"=character(),
                   timings=character(),
                   Date=as.Date(character()),
                   CurrDate = as.Date(character()),
                   RT = as.character(),
                   n_day = as.numeric(),
                   stringsAsFactors=TRUE) 

for ( i in 1:nrow(input)){
  b <- clean(input[i,1], input[i,2])
  day <- rbind(day,b)
}


day3 <- as.data.frame(clean("12062018" , 1))

esquisser(day3[,c(1:11)])

unique(day3$n_day)

day$wk_day <- weekdays(day$CurrDate)

#Lets analyse for one day in the future 
as.data.frame(day %>%
                select(Date, CurrDate, price) %>%
                group_by(Date, CurrDate) %>%
                summarize(cheap_price = min(price)) )-> cheapest_day

#Lets analyse fare for a day 12/31/2018
oneDay <- cheapest_day[which(cheapest_day$Date == "01/1/2019"),]
cp_xts3 <- ts(oneDay$cheap_price)

#There is a weekly trend that exists
plot.ts(cp_xts3)

#Lets see why this trend exits
lm <- lm(price ~ n_day , data = day)


#Saving the file 
write.csv(day,"Data/14_day.csv")


file:///C:/Users/Lenovo/Downloads/AirFair/06Dec_night.xlsx

#Reading data
day <- read.csv("Data/14_day.csv")
day <- day[c("airline", "arrival", "departure", "plane" , "stops", "price", "wk_day" , "n_day" ,"Date" , "CurrDate"  )]

day[grep(pattern = "Endeavor Air DBA Delta Connection|Republic|Skywest|NA", day$airline),1]  <- "NA"

#Cheapest fare of airline for the day 
day %>% 
  select(Date, airline , CurrDate , price , n_day , wk_day ) %>%
  group_by ( airline, Date , CurrDate , n_day , wk_day) %>%
  summarise( ch_price = min(price)) -> cheap_day

#Graphing for different airlines for fare on a partiular day
cheap_day_one <- cheap_day[which(cheap_day$Date == unique(cheap_day$Date)[100]),]

#50 
#1

ggplot(cheap_day_one, aes(x = n_day, y = ch_price, group = airline)) + 
  geom_line() + 
  facet_wrap(~ airline, scales = 'free_y', ncol = 1)

View(cheap_day_one)

#Expecting very less variance inside groups of airlines. Considering random effects on n_day of airline carriers. 
#Use : Help us evaluate for airlines that are not present in data

#Since there is a non-linear relationship in the data, we will allow weekday effects to be categorical. 
#Since airlines might be introducing offers on different days, we will allow for random effects of airline carrier on the days
#In order to 

#Setting up a traina and test framework to validate the dataset
#Out of time frame validation will be done 
#The model will be built currently on first 2 weeks data 
#We Will use the current price to simulate for prices in the next 5 days and check for price differences 

#Train data : day 
#Test data : Dec 2 - Dec 6
#Test statistic to be used : abs( Price predicted - Actual price )

#Transforming variables for better prediction
#Since everyday price varies a lot acc to traffic, and we can model it according to traffic data, but in order to bypass this - 

#We will model Perentage change in price on respective day 
#If in case we dont have price on just previous day, we will calc percentage difference wrt last day and split it equally over the entire duration. 


#Removing NA

cheap_day <- cheap_day[which (cheap_day$airline %in% c("Alaska Airlines", "American Airlines" , "Delta", "JetBlue Airways" , "Sun Country Airlines" , "Spirit Airlines" , "United" ) ),]

write.csv(cheap_day, "Data/cheap_day.csv")

cheap_day <- read.csv("Data/cheap_day.csv" , row.names = 1)

#Creating transformed variable which is = (y-y_previous)/y_previous
#Airline 1 


#Number of unique days 

dt <- unique(cheap_day$Date)
ch_day <- data.frame()

for ( i in 1:length(dt)){
  ch_day_one <- cheap_day[cheap_day$Date ==  dt[i] , ]
  mu <- mean(ch_day_one$ch_price)
  nd <- unique(ch_day_one$n_day)
  for ( j in 1:length(nd)){
    x <- mean( ch_day_one[ ch_day_one$n_day == nd[j]  , c("ch_price")]  )
    ch_day_one[ ch_day_one$n_day == nd[j]  , c("ch_price")]<- ch_day_one[ch_day_one$n_day == nd[j]  , c("ch_price")] / min(max(round(x/mu + 0.2),1),2)
    print(min(max(round(x/mu + 0.2),1),2))
  }
  ch_day <- rbind(ch_day, ch_day_one)
}

cheap_day <- ch_day

sum(cheap_day$ch_price == ch_day$ch_price)

ch_day_one <- ch_day[ch_day$Date == "02/12/2019", ]
cheap_day_one <- cheap_day[cheap_day$Date == "02/12/2019", ]
esquisser(ch_day_one)
esquisser(cheap_day_one)

write.csv(ch_day, "Code/ch_day.csv")

cheap_day <- read.csv("Data/ch_day.csv" , row.names = 1)

df_per <- data.frame()
airline <- unique(cheap_day$airline)
for ( k in 1:length(airline)){
  al <- cheap_day[which (cheap_day$airline == airline[k]) , ]
  dt <- unique(al$Date)
  price <- data.frame()
  for ( j in 1:length(dt)){
    price_od <- al[which(al$Date == dt[j] ),]
    price_od <- price_od[order(price_od$n_day),]
    
    pd_price_od <- diff(price_od$ch_price)/price_od$ch_price[-length(price_od$ch_price)] * 100
    n_od <- -1 * diff(price_od$n_day)
    avg_pd_od <- pd_price_od/n_od
    price_od$avg_pr <- c(0,avg_pd_od)
    price_od$avg_pr <- as.numeric(price_od$avg_pr)
    price_od <- price_od[which (!(price_od$avg_pr > 20)),]
    price_od <- price_od[which (!(price_od$avg_pr < -20)),]
    avg_pd_od <- price_od$avg_pr
    avg_pd_od <- c(0,avg_pd_od,0,0)
    mean_pd_od <- SMA(avg_pd_od, n = 3)[-c(1:2, length(avg_pd_od))]
    price_od$mean_pd_od <- mean_pd_od
    price <- rbind(price,price_od)
    }
  df_per <- rbind(df_per, price)
}
write.csv(df_per, "Code/df_per.csv")
write.csv(df_per, "Data/df_per.csv")

max(df_per$mean_pd_od)

plot(df_per$mean_pd_od ,  df_per$n_day)

esquisser(df_per)

df_per$mean_pd_od <- 

#Transformed var2  = moving 3 days average of change
#Will are using this transformation as we dont know when the actual effect would have taken place

#Modelling variable for change: 
cheap_day <- read.csv("Data/cheap_day.csv", row.names = 1)
unique(cheap_day$airline)


fit <- lmer (mean_pd_od ~ n_day ,data = cheap_day)

x$name[order(x$val)]
  
esquisser(cheap_day)
library(ggplot2)


graph_df <- cheap_day[which(cheap_day$ch_price < 500),]

levels(graph_df$wk_day ) <- weekdays(min(as.Date(graph_df$CurrDate , "%Y-%m-%d")) + 0:6)

esquisser(graph_df)
library(ggplot2)

#Shows variation by day of booking

ggplot(data = df_per) +
  aes(x = wk_day, y = ch_price, fill = airline) +
  geom_boxplot() +
  labs(title = "Variation of cheapest ticket prices by day of booking",
    x = "Day of Booking",
    y = "Ticket Price") +
  theme_minimal()

#Transformation on ticket price

hist(df_per$avg_pr , breaks = 50)




esquisser(df_per)
library(ggplot2)

levels(df_per$wk_day ) <- weekdays(min(as.Date(df_per$CurrDate , "%Y-%m-%d")) + 0:6)

ggplot(data = df_per) +
  aes(x = wk_day, y = mean_pd_od, color = airline) +
  geom_boxplot(fill = "#0c4c8a") +
  theme_minimal()

df_new <- df_per[which(df_per$mean_pd_od < 50 & df_per$mean_pd_od >  -50),]
#Not considering cases where the change is very high

df_per$week <- as.factor(round ( df_per$n_day / 7 , 0))

summary(lmer ( mean_pd_od ~  (1|Date) + week + (scale(n_day)|week) + wk_day,data = df_per))

df_new <- df_per[df_per$ch_price < 1000 , ]
df_new$wk_num <- as.factor(round ( df_new$n_day / 7 , 0))

plot(predict(lmer ( mean_pd_od ~ wk_day + airline  + (wk_day|airline) + wk_num ,data = df_new)) , df_new$mean_pd_od)


#Trying to see if one should wait?  
#Building a multinomial logistic model

