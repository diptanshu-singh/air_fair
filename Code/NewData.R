library(readxl)

setwd("Data/NewData")

xl <- list.files()

#Select xlsx_files
xl <- xl[grep('xlsx',xl)]

clean <- function(dt, obj){
  obj <- as.numeric(obj)
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
    a <- as.data.frame(read_xlsx(xl[obj], sheet = i, n_max = 50))[,-1]
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


input <- as.data.frame(cbind( c( paste0("120",2:8,"2018") ),  c(1,2,3,4,5,6,7)) , stringsAsFactors = FALSE)



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


day$wk_day <- weekdays(day$CurrDate)

write.csv(day,"day_nd.csv")
day <- read.csv("day_nd.csv", row.names = 1)

day %>% 
  select(Date, airline , CurrDate , price , n_day , wk_day ) %>%
  group_by ( airline, Date , CurrDate , n_day , wk_day) %>%
  summarise( av_price = min(price )) -> avg_day


av_day <- data.frame()

avg_day$select <- 1

dt <- unique(avg_day$Date)

#Outlier detection on average ticket price
for ( i in 1:length(dt)){
  av_day_one <- as.data.frame(avg_day[avg_day$Date ==  dt[i], ])
  mu <- mean(av_day_one$av_price)
  sd <- sd(av_day_one$av_price)
  nd <- unique(av_day_one$n_day)
  for ( j in 1:length(nd)){
    x <- mean( av_day_one[ av_day_one$n_day == nd[j]  , c("av_price")]  )
    if( x > mu + 0.5*sd){
      avg_day[ avg_day$Date ==  dt[i] & avg_day$n_day == nd[j],7 ] <- 0 
    }
  }
  print(dt[i])
}

av_day <- avg_day


av_day <- av_day[which (av_day$airline %in% c("Alaska Airlines", "American Airlines" , "Delta", "JetBlue Airways" , "Sun Country Airlines" , "Spirit Airlines" , "United" ) ),]

write.csv(av_day , "avg_day_nd.csv")

avg_day <- read.csv("avg_day_nd.csv" , row.names = 1)
#Working on avg_day data
#Outlier 
mahal = mahalanobis(avg_day[, c(6,4)] , 
                    colMeans(avg_day[, c(6,4)], na.rm = TRUE) ,
                    cov(avg_day[, c(6,4)] ))


cutoff = qchisq( 1 - 0.001 ,  ncol(avg_day[, c(6,4)]))


avg_day_o <- avg_day [( mahal < cutoff),]


avg_day_o$week <- as.factor(round(avg_day_o$n_day / 7 ))


avg_day <- avg_day_o

avg_day <- read.csv("avg_day_nd.csv" , row.names = 1)

ch_day <- avg_day
colnames(ch_day) [6]  <- "ch_price"

ch_day <- ch_day[ch_day$select == 1, ]

write.csv(ch_day , "ch_day_nd.csv")

ch_day <- read.csv("ch_day_nd.csv" , row.names = 1)

#We want to predict this 



