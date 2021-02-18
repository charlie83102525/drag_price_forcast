library(lubridate)
library(dplyr)
library(magrittr)
library(zoo)
library(forecast)
yzc_d <- yzc_new
yzc_d <-mutate(yzc_d,Year = year(yzc_d$invoiceDate))
yzc_d <- mutate(yzc_d,Month = month(yzc_d$invoiceDate))
yzc_d <- mutate(yzc_d,Day = day(yzc_d$invoiceDate))
str(yzc_d)
# $ dem                : Factor w/ 2 levels "Domestic","Export": 1 1 1 1 2 2 2 2 2 2 ...
# $ area               : Factor w/ 5 levels "AFR","AME","ASIA",..: 5 5 5 5 3 3 3 3 3 3 ...
# $ country            : Factor w/ 37 levels "AE","AT","BD",..: 34 34 34 34 7 7 7 7 7 7 ...
# $ customerCode       : num [1:12048] 1e+07 1e+07 1e+07 1e+07 1e+07 ...
# $ customer           : Factor w/ 389 levels "A.T. INTERNATIONAL",..: 307 357 321 309 278 278 278 278 278 278 ...
# $ invoiceDate        : chr [1:12048] "2010/1/5" "2010/1/5" "2010/1/5" "2010/1/7" ...
# $ productCode        : Factor w/ 28 levels "ACV","ALA","CLD",..: 14 8 26 16 26 26 26 26 26 26 ...
# $ productName        : Factor w/ 23 levels "80% Sodium Starch Glycolate + 19% Vital Wheat Gluten + 1% Dextrin",..: 16 7 21 13 21 21 21 21 21 21 ...
# $ indications        : Factor w/ 13 levels "\nIron supplement",..: 2 3 10 3 10 10 10 10 10 10 ...
# $ deliverDate        : chr [1:12048] "2010/1/5" "2010/1/5" "2010/1/5" "2010/1/7" ...
# $ totalntd           : num [1:12048] 23000 34000 8500 214200 221444 ...
# $ transportConditions: Factor w/ 8 levels "CFR","CIF","CIP",..: NA NA NA NA 2 2 2 2 2 2 ...
# $ port               : Factor w/ 74 levels "AIR","AIR Algiers",..: NA NA NA NA 64 64 64 64 64 64 ...
# $ Year               : num [1:12048] 2010 2010 2010 2010 2010 2010 2010 2010 2010 2010 ...
# $ Month              : num [1:12048] 1 1 1 1 1 1 1 1 1 1 ...
# $ Day                : int [1:12048] 5 5 5 7 7 7 7 7 7 7 ...

#將chr轉成factor
yzc_d$dem <- factor(yzc_d$dem)
yzc_d$area <- factor(yzc_d$area)
yzc_d$country <- factor(yzc_d$country)
yzc_d$customer <- factor(yzc_d$customer)
yzc_d$productCode <- factor(yzc_d$productCode)
yzc_d$productName <- factor(yzc_d$productName)
yzc_d$indications <- factor(yzc_d$indications)
yzc_d$transportConditions <- factor(yzc_d$transportConditions)
yzc_d$port <- factor(yzc_d$port)


#group_by年月
salesALL <- yzc_d %>% group_by(Year,Month) %>% summarise('salesALL' = sum(totalntd))
salesALL01 <- salesALL %>% filter(Year > 2015) #篩選2016年以後資料

#轉成時間序列
sellTs <- ts(salesALL$salesALL, frequency = 12, start = c(2010,1))
sellTs01 <-ts(salesALL01$salesALL, frequency = 12, start = c(2016,1))  

#建模
auto.arima(log(sellTs),trace= T)
sellarima1 <- arima(log(sellTs),order=c( 0 , 1 , 1 ),
                   seasonal=list(order=c( 0 , 1 , 1 ),period= 12 ), 
                   method= "ML" ) 
sellForecast <- forecast(sellarima1, h= 12 ,level=c( 99.5 ))
sellForecast60 <- forecast(sellarima1, h= 60 ,level=c( 99.5 ))
plot(sellTs)

foreSell <- auto.arima( x = sellTs)
predict(foreSell, n.ahead = 12, se.fit = TRUE)
theForecast <- forecast(object = foreSell, h = 12)
plot(theForecast)

#五年內資料
auto.arima(sellTs,trace= T)
auto.arima(log(sellTs01),trace= T)
arima01 <- arima(log(sellTs01),order=c( 0 , 1 , 1 ),
                 seasonal=list(order=c( 2 , 1 , 2 ),period= 12 ),
                 method= "ML" )
forecast01 <- forecast(arima01, h= 15 ,level=c( 99.5 ))
plot(forecast01)

#五個區域的銷售預測

#國內的銷售預測
dem_sales <- yzc_d %>%group_by(Year,Month,dem) %>% summarise('salesALL' = sum(totalntd))
dem_sales_d <- dem_sales %>% filter(dem == 'Domestic')
dem_sales_d <- dem_sales_d[ ,-3]
sellTs_d <- ts(dem_sales_d$salesALL, frequency = 12, start = c(2010,1))

auto.arima(sellTs_d,trace= T)
arima_d <- arima(log(sellTs_d),order=c( 0 , 1 , 1 ),
                 seasonal=list(order=c( 2 , 1 , 2 ),period= 12 ),
                 method= "ML" )
forecast_d <- forecast(arima_d, h= 15 ,level=c( 99.5 ))
plot(forecast_d, main ='Domestic')



dem_sales_e <- dem_sales %>% filter(dem == 'Export')
dem_sales_e <- dem_sales_e[ ,-3]
sellTs_e <- ts(dem_sales_e$salesALL, frequency = 12, start = c(2010,1))

auto.arima(sellTs_e,trace= T)
arima_e <- arima(log(sellTs_e),order=c( 0 , 1 , 1 ),
                 seasonal=list(order=c( 2 , 1 , 2 ),period= 12 ),
                 method= "ML" )
forecast_e <- forecast(arima_e, h= 15 ,level=c( 99.5 ))
plot(forecast_e, main ='Export')

#前五大客戶預測
yzc_cus <- yzc_d %>% group_by(customer) %>% summarise('Sum' = sum(totalntd))
yzc_cus_top <- top_n(yzc_cus, 3, Sum) %>% arrange(desc(Sum))
cus_sales <- yzc_d %>%group_by(Year,Month,customer) %>% summarise('salesALL' = sum(totalntd))

#top1
cus_sales_1 <- cus_sales %>% filter(customer == 'CARLSBAD TECHNOLOGY INC.') 
cus_sales_1 <- cus_sales_1[ ,-3]

sellTs_cus_1 <- ts(cus_sales_1$salesALL, frequency = 12, start = c(2010,1))

auto.arima(sellTs_cus_1,trace= T)
arima_cus_1 <- arima(log(sellTs_cus_1),order=c( 0 , 1 , 1 ),
                     seasonal=list(order=c( 2 , 1 , 2 ),period= 12 ),
                     method= "ML" )
forecast_cus_1 <- forecast(arima_cus_1, h= 15 ,level=c( 99.5 ))
plot(forecast_cus_1, main ='CARLSBAD TECHNOLOGY INC.')


#前三名藥品銷售預測
yzc_spe <- yzc_d %>% group_by(productName) %>% summarise('Sum' = sum(totalntd))
yzc_spe_top <- top_n(yzc_spe, 3, Sum) %>% arrange(desc(Sum))
spe_sales <- yzc_d %>%group_by(Year,Month,productName) %>% summarise('salesALL' = sum(totalntd))
#top1
spe_sales_1 <- spe_sales %>% filter(productName == 'Sodium Starch Glycolate') 
spe_sales_1 <- spe_sales_1[ ,-3]

sellTs_spe_1 <- ts(spe_sales_1$salesALL, frequency = 12, start = c(2010,1))

auto.arima(sellTs_spe_1,trace= T)
arima_spe_1 <- arima(log(sellTs_spe_1),order=c( 0 , 1 , 1 ),
                 seasonal=list(order=c( 2 , 1 , 2 ),period= 12 ),
                 method= "ML" )
forecast_spe_1 <- forecast(arima_spe_1, h= 15 ,level=c( 99.5 ))
plot(forecast_spe_1, main ='Sodium Starch Glycolate')

#top2
spe_sales_2 <- spe_sales %>% filter(productName == 'Diclofenac Sodium')
spe_sales_2 <- spe_sales_2[ ,-3]
sellTs_spe_2 <- ts(spe_sales_2$salesALL, frequency = 12, start = c(2010,1))
auto.arima(sellTs_spe_2,trace= T)
arima_spe_2 <- arima(log(sellTs_spe_2),order=c( 0 , 1 , 1 ),
                     seasonal=list(order=c( 2 , 1 , 2 ),period= 12 ),
                     method= "ML" )
forecast_spe_2 <- forecast(arima_spe_2, h= 15 ,level=c( 99.5 ))
plot(forecast_spe_2, main ='Diclofenac Sodium')

#top3
spe_sales_3 <- spe_sales %>% filter(productName == 'Mefenamic Acid')
spe_sales_3 <- spe_sales_3[ ,-3]
sellTs_spe_3 <- ts(spe_sales_3$salesALL, frequency = 12, start = c(2010,1))
auto.arima(sellTs_spe_3,trace= T)
arima_spe_3 <- arima(log(sellTs_spe_3),order=c( 0 , 1 , 1 ),
                     seasonal=list(order=c( 2 , 1 , 2 ),period= 12 ),
                     method= "ML" )
forecast_spe_3 <- forecast(arima_spe_3, h= 15 ,level=c( 99.5 ))
plot(forecast_spe_3, main ='Mefenamic Acid')

