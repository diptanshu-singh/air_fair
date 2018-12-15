# Normalization of y 

cheap_day <- cheap_day <- read.csv("Data/cheap_day.csv" , row.names = 1)

dt <- unique(cheap_day$Date)
ch_day <- data.frame()

for ( i in 1:length(dt)){
  ch_day_one <- cheap_day[cheap_day$Date ==  dt[i], ]
  mu <- mean(ch_day_one$ch_price)
  nd <- unique(ch_day_one$n_day)
  for ( j in 1:length(nd)){
    x <- mean( ch_day_one[ ch_day_one$n_day == nd[j]  , c("ch_price")]  )
    ch_day_one[ ch_day_one$n_day == nd[j]  , c("ch_price")]<- ch_day_one[ch_day_one$n_day == nd[j]  , c("ch_price")] / min(max(round(x/mu + 0.2),1),2)
  }
  ch_day <- rbind(ch_day, ch_day_one)
}

write.csv(ch_day , "Data/ch_day.csv")
write.csv(ch_day , "Code/ch_day.csv")

cheap_day <- read.csv("Code/ch_day.csv" , row.names = 1)

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
    avg_pd_od <- c(0,0,avg_pd_od,0,0)
    mean_pd_od <- SMA(avg_pd_od, n = 3)[-c(1:2, length(avg_pd_od))]
    price_od$mean_pd_od <- mean_pd_od
    price <- rbind(price,price_od)
  }
  df_per <- rbind(df_per, price)
}

#Analysis of avergae ticket prices 
day %>% 
  select(Date, airline , CurrDate , price , n_day , wk_day ) %>%
  group_by ( airline, Date , CurrDate , n_day , wk_day) %>%
  summarise( av_price = mean(price)) -> avg_day

write.csv(avg_day , "Data/avg_price.csv")

#Removing prices that are of very high range 

esquisser(av_day_one)

dt <- unique(avg_day$Date)

av_day <- data.frame()

avg_day$select <- 1

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

max(cheap_day$CurrDate)

#Selecting airlines
av_day <- av_day[which (av_day$airline %in% c("Alaska Airlines", "American Airlines" , "Delta", "JetBlue Airways" , "Sun Country Airlines" , "Spirit Airlines" , "United" ) ),]

write.csv(av_day , "Code/avg_day.csv")

esquisser(av_day)

#Working on avg_day data
#Outlier 
mahal = mahalanobis(avg_day[, c(6,4)] , 
                    colMeans(avg_day[, c(6,4)], na.rm = TRUE) ,
                    cov(avg_day[, c(6,4)] ))


cutoff = qchisq( 1 - 0.001 ,  ncol(avg_day[, c(6,4)]))


avg_day_o <- avg_day [( mahal < cutoff),]

#Checking for multicolinerlity 
cor(avg_day_o[, c(6,4)])
symnum(cor(avg_day_o[, c(6,4)]))


avg_day_o$week <- as.factor(round(avg_day_o$n_day / 7 ))
plot(avg_day_o$av_price , avg_day_o$week)


#Checking assumptions
random <- rchisq( n = nrow(avg_day_o) , df = 7)
fake <- lm(random ~ ., data = avg_day_o)
fitted = scale(fake$fitted.values)
standardized = rstudent(fake)  

#Linearity 
qqnorm(standardized)
abline(0,1)
hist(standardized)


avg_day <- avg_day_o

avg_day$week <- factor(round(avg_day$n_day / 7 ))
avg_day$tri_d <- factor(round(avg_day$n_day / 3 )) 

library(lme4)

m1 <- lm( av_price ~ week + Date + wk_day + airline, data = avg_day )

AIC(m1)

m2 <- (lmer( av_price ~ week + (1|Date) + wk_day + airline, data = avg_day ))

AIC(m2)

plot(predict(fit), avg_day$av_price)

avg_day$tp <- 1
avg_day[avg_day$n_day > 3, 10 ] <- 2 
avg_day[avg_day$n_day > 17, 10] <- 3
avg_day[avg_day$n_day > 35, 10] <- 4
avg_day[avg_day$n_day > 55, 10] <- 5


write.csv(avg_day , "Code/avg_day.csv")

















