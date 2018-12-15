library(readxl)

setwd("C:\\Users\\Lenovo\\Documents\\R\\air_fair")

day <- read.csv("Code\\14_day.csv", row.names = 1)

day %>% 
  select(Date, airline , CurrDate , price , n_day , wk_day ) %>%
  group_by ( airline, Date , CurrDate , n_day , wk_day) %>%
  summarise( av_price = mean(price )) -> avg_day


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
av_day <- av_day[av_day$select == 1, ]
av_day <- av_day[which (av_day$airline %in% c("Alaska Airlines", "American Airlines" , "Delta", "JetBlue Airways" , "Sun Country Airlines" , "Spirit Airlines" , "United" ) ),]
write.csv(av_day , "Code\\avg_day.csv")

avg_day <- read.csv("Code\\avg_day.csv" , row.names = 1)
#Working on avg_day data
#Outlier 
mahal = mahalanobis(avg_day[, c(6,4)] , 
                    colMeans(avg_day[, c(6,4)], na.rm = TRUE) ,
                    cov(avg_day[, c(6,4)] ))


cutoff = qchisq( 1 - 0.001 ,  ncol(avg_day[, c(6,4)]))


avg_day_o <- avg_day [( mahal < cutoff),]


avg_day_o$week <- as.factor(round(avg_day_o$n_day / 7 ))



avg_day <- avg_day_o

avg_day$week <- factor(round(avg_day$n_day / 7 ))
avg_day$tri_d <- factor(round(avg_day$n_day / 3 )) 

write.csv(avg_day , "Code\\avg_day.csv")
avg_day <- read.csv("Code\\avg_day.csv" , row.names = 1)


avg_day$tp <- 1
avg_day[avg_day$n_day > 3, 10 ] <- 2 
avg_day[avg_day$n_day > 17, 10] <- 3
avg_day[avg_day$n_day > 35, 10] <- 4
avg_day[avg_day$n_day > 55, 10] <- 5


write.csv(avg_day , "Code/avg_day.csv")


#Done with cleaned training data 






### Create clean test data 

setwd("C:\\Users\\Lenovo\\Documents\\R\\air_fair\\Data\\NewData")


day <- read.csv("day_nd.csv", row.names = 1)

day %>% 
  select(Date, airline , CurrDate , price , n_day , wk_day ) %>%
  group_by ( airline, Date , CurrDate , n_day , wk_day) %>%
  summarise( av_price = mean(price )) -> avg_day


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

avg_day <- avg_day[avg_day$select == 1 , ]

write.csv(avg_day_train , "Code\\avg_day.csv")


setwd("C:\\Users\\Lenovo\\Documents\\R\\air_fair")

###Datasets : 
#Average ticket prices 
#Train 
avg_day_train <- read.csv("Code\\avg_day.csv" , row.names = 1)

#Test
avg_day_test <- read.csv("Data\\NewData\\avg_day_nd.csv" , row.names = 1)

#Cheapest ticket price
#Train 
ch_day_train <- read.csv("Code\\ch_day.csv" , row.names = 1)


#Test 
ch_day_test <- read.csv("Data\\NewData\\ch_day_nd.csv" , row.names = 1)

#Model building 

avg_day_train$week <- factor(round(avg_day_train$n_day / 7 ))
avg_day_train$tp   <- factor(avg_day_train$tp)

library(rstanarm)
m1 <- stan_lmer( y_trans ~ (1|Date) + n_day + week + airline + wk_day  + (scale(n_day)|tp), data = avg_day_train)
save(m1, file = "model1.RData")


summary(m1)

library("shinystan")

shinystan::launch_shinystan(m1)


#Warming up 


avg_day_train$y_trans <- log(sqrt(avg_day_train$av_price - min(avg_day_train$av_price) + 1 ))
avg_day_test$y_trans <- log(sqrt(avg_day_test$av_price - min(avg_day_test$av_price) + 1 ))



y_test <- posterior_predict(m1, avg_day_test)

plot(y_test , avg_day_test$av_price)
dim(y_test)

loo_bglm_1 <- loo(m1 , k_threshold = 0.7)


posterior <- as.matrix(fit)

library(bayesplot)

plot_title <- ggtitle("Effect of coefficients")

coef(m1)

a <- mcmc_areas(coef(m1), 
                pars = all, 
                prob = 0.8) + plot_title

plot(a)
colnames(avg_day_train)



train <- left_join(x = avg_day_train , y = ch_day_train , by = c("airline","Date")  , by.x = T)

colnames(train)
mod2 <- train[,c("airline"  ,  "Date" ,"n_day.x", "week" ,"av_price" ,"ch_price")]

lm(ch_price ~ av_price , data = mod2)

lm(ch_price ~ av_price + week, data = mod2)

m2 <- lm(ch_price ~ av_price + as.factor(week), data = mod2)


#Predict
avg_day_test$week <- factor(round(avg_day_test$n_day / 7 ))
avg_day_test$tp   <- factor(avg_day_test$tp)


pp <- posterior_predict(m1, avg_day_test)


min(avg_day_train$av_price)
min(avg_day_test$av_price)


avg_day_test$pred <-0 
avg_day_test$med <- 0 
avg_day_test$q25 <- 0 
avg_day_test$q75 <- 0
  
  
find 

for (i in 1:3028){
  y_mean <- mean(pp[,i])
  avg_day_test[i,11] <- exp(2 * y_mean)  + min( avg_day_train$av_price) 
  
  
  y_median <- median(pp[,i])
  avg_day_test[i,12] <- exp(2 * y_median)  + min( avg_day_train$av_price) 
  
  
  
  y_25 <- quantile(pp[,i], probs = c(0.25))
  avg_day_test[i,13] <- exp(2 * y_25)  + min( avg_day_train$av_price) 
  
  
  y_75 <- quantile(pp[,i], probs = c(0.75))
  avg_day_test[i,14] <- exp(2 * y_75)  + min( avg_day_train$av_price) 
  
  
}


colnames(ch_day_test)[c(-6)]

test <- left_join(x = avg_day_test , y = ch_day_test , by =   colnames(ch_day_test)[c(-6)]  , by.x = T)


write.csv(avg_day_test , "Code/avg_day_predicted.csv")

as.data.frame(test %>%
  filter ( CurrDate == "2018-12-02" ) %>%
  group_by( Date) %>%
  summarise (naive = min(ch_price) ) ) -> naive



Final <- left_join(strategy1 , naive , by= "Date" )
Final <- left_join(strategy2 , Final , by= "Date" )


Final$ch <- ifelse(Final$strategy.x < Final$strategy.y , Final$strategy.x  , Final$strategy.y)

Final$diff <- Final$ch - Final$naive

Final$diff

hist(Final$diff , na.rm = T , breaks = 20)

write.csv(Final, "Code/Compare.csv")

  unique(test$CurrDate)
