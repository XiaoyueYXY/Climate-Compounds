# Do time series for RQ2 #
library("here")
library("dplyr")
library("lubridate")
library("xts")
library("forecast")
library("ggplot2")
library("ggthemes")
library("lmtest")
library("tseries")
library("imputeTS")

`%!in%` = Negate(`%in%`)

#for tutorials, see e.g.:
#https://nicolarighetti.github.io/Time-Series-Analysis-With-R/intervention-analysis.html
#https://medium.com/@mouse3mic3/a-practical-guide-to-arima-with-auto-arima-function-in-r-252aa84232af
#https://otexts.com/fpp2/arima.html

#for introductory pieces, see e.g.:
#https://bmcmedresmethodol.biomedcentral.com/articles/10.1186/s12874-021-01235-8#Sec2 & R Code
#https://doi.org/10.1016/j.jclinepi.2018.05.026
#https://academic.oup.com/ije/article/46/1/348/2622842?login=false
#https://books.google.de/books?hl=de&lr=&id=bACvDwAAQBAJ&oi=fnd&pg=PP1&dq=%22interrupted+time+series%22&ots=BJTfPPcjSe&sig=_21X0Yw4dJNLYb84drIUeJ0u5Xo#v=onepage&q=%22interrupted%20time%20series%22&f=false

#for examples from comm sci, see e.g.:
#https://www.tandfonline.com/doi/full/10.1080/19312450902809672
#https://www.tandfonline.com/doi/full/10.1080/10810730.2018.1528318
#https://www.degruyter.com/document/doi/10.1515/omgc-2023-0012/html
#https://academic.oup.com/joc/article/73/1/73/6849391#393973361

#### Step 1: Load data ####
data <- read.csv(here("data", paste0("output.csv"))) %>%
  select("outlet", "year", "month", "neutral_binary", "urgent_binary")

#### Step 2: Prepare data ####

#transform date variable
data <- data %>%
  
  #correct numbers for months
  mutate(month = paste0(0, month),
         month = replace(month, month == "010", "10"),
         month = replace(month, month == "011", "11"),
         month = replace(month, month == "012", "12"),
         date = paste0(year, "/", month, "/", "1"),
         date = as.Date(date, format = "%Y/%m/%d"),
         
         #transform to monthly time series
         date = zoo::as.yearmon(date),
         
         #include country indicator
         country = "Australia",
         country = replace(country,
                           outlet %in% c("GlobeMail", "Toronto Star"),
                           "Canada"),
         country = replace(country,
                           outlet %in% c("Hindu", "Times of India"),
                           "India"),
         country = replace(country,
                           outlet %in% c("The New Zealand Herald", "The Press"),
                           "New Zealand"),
         country = replace(country,
                           outlet %in% c("Sunday Times", "The Star"),
                           "South Africa"),
         country = replace(country,
                           outlet %in% c("Bangkok Post", "The Nation"),
                           "Thailand"),
         country = replace(country,
                           outlet %in% c("Guardian", "The Times"),
                           "UK"),
         country = replace(country,
                           outlet %in% c("The New York Times", "The Washington Post"),
                           "USA"))

#### Step 3: Time series: Australia ####

##### 3.1 Prepare time series #####

#aggregate use of urgent labels across outlets
ts_australia <- data %>%

  #filter for Australian coverage
  filter(country == "Australia") %>%
  
  #group by date
  group_by(date) %>%

  #create relative share of coverage with urgent labels
  summarize(urgent_binary = sum(urgent_binary)/n()*100) %>%
  ungroup() %>%
  
  #sort by date for extraction of time series
  arrange(date) 

#add missing values (for months without coverage)
ts_australia <- rbind(ts_australia, data %>%
              filter(date %!in% ts_australia$date) %>%
              pull("date") %>%
              unique() %>%
              data.frame("date" = zoo::as.yearmon(.),
                         "urgent_binary" = NA) %>%
              select("date", "urgent_binary")) %>%
  arrange(date)

##### 3.2 Check assumptions & visualize #####

#check
ggtsdisplay(ts_australia$urgent_binary, plot.type = "partial", 
            points = FALSE, lag.max = 250, smooth = TRUE,
            main = "Use of Urgent Labels across Time",
            xlab = "Units of Time (Months)",
            ylab = "% of articles using labels",
            theme = theme_hc())

#transform to time series format: 12 months, starting in 2006
x <- ts(ts_australia$urgent_binary, frequency = 12, start = c(2006, 1))

#inspect shift after Guardian policy virtually, date of implementation: May 2019
#source: https://www.theguardian.com/environment/2019/may/17/why-the-guardian-is-changing-the-language-it-uses-about-the-environment)
plot(x)
abline(v=2019, col = "gray", lty = "dashed", lwd=2)

#independent variable 1: step change or level shift (i.e., shift of mean after May 2019)
guardian_edit <- as.numeric(as.yearmon(time(x)) >= "Mai 2019")

#independent variable 2: ramp (i.e., linear increase after May 2019)
increase <- c(rep(0, 160), seq(1, 204-160, 1))

##### 3.3 Model time series #####

# Use automated approach to identify parameters
model_urgent <- auto.arima(x, 
                           xreg = cbind(guardian_edit, increase),
                           stepwise = FALSE,
                           trace = TRUE)

# Check residuals
checkresiduals(model_urgent)

#no autocorrelation in residuals?
Box.test(model_urgent$residuals, lag = 24, type = "Ljung-Box")

#check result
coeftest(model_urgent)

#### Step 4: Time series: Canada ####

##### 4.1 Prepare time series #####

#aggregate use of urgent labels across outlets
ts_canada <- data %>%
  
  #filter for Canadian coverage
  filter(country == "Canada") %>%
  
  #group by date
  group_by(date) %>%
  
  #create relative share of coverage with urgent labels
  summarize(urgent_binary = sum(urgent_binary)/n()*100) %>%
  ungroup() %>%
  
  #sort by date for extraction of time series
  arrange(date) 

##### 4.2 Check assumptions & visualize #####

#check
ggtsdisplay(ts_canada$urgent_binary, plot.type = "partial", 
            points = FALSE, lag.max = 250, smooth = TRUE,
            main = "Use of Urgent Labels across Time",
            xlab = "Units of Time (Months)",
            ylab = "% of articles using labels",
            theme = theme_hc())

#transform to time series format: 12 months, starting in 2006
x <- ts(ts_canada$urgent_binary, frequency = 12, start = c(2006, 1))

#inspect shift after Guardian policy virtually, date of implementation: May 2019
plot(x)
abline(v=2019, col = "gray", lty = "dashed", lwd=2)

##### 4.3 Model time series #####

# Use automated approach to identify parameters
model_urgent <- auto.arima(x, 
                           xreg = cbind(guardian_edit, increase),
                           stepwise = FALSE,
                           trace = TRUE)

# Check residuals
checkresiduals(model_urgent)

#no autocorrelation in residuals?
Box.test(model_urgent$residuals, lag = 24, type = "Ljung-Box")

#check result
coeftest(model_urgent)

#### Step 5: Time series: India ####

##### 5.1 Prepare time series #####

#aggregate use of urgent labels across outlets
ts_india <- data %>%
  
  #filter for Indian coverage
  filter(country == "India") %>%
  
  #group by date
  group_by(date) %>%
  
  #create relative share of coverage with urgent labels
  summarize(urgent_binary = sum(urgent_binary)/n()*100) %>%
  ungroup() %>%
  
  #sort by date for extraction of time series
  arrange(date) 

##### 5.2 Check assumptions & visualize #####

#check
ggtsdisplay(ts_india$urgent_binary, plot.type = "partial", 
            points = FALSE, lag.max = 250, smooth = TRUE,
            main = "Use of Urgent Labels across Time",
            xlab = "Units of Time (Months)",
            ylab = "% of articles using labels",
            theme = theme_hc())

#transform to time series format: 12 months, starting in 2006
x <- ts(ts_india$urgent_binary, frequency = 12, start = c(2006, 1))

#inspect shift after Guardian policy virtually, date of implementation: May 2019
plot(x)
abline(v=2019, col = "gray", lty = "dashed", lwd=2)

##### 5.3 Model time series #####

# Use automated approach to identify parameters
model_urgent <- auto.arima(x, 
                           xreg = cbind(guardian_edit, increase),
                           stepwise = FALSE,
                           trace = TRUE)

# Check residuals
checkresiduals(model_urgent)

#no autocorrelation in residuals?
Box.test(model_urgent$residuals, lag = 24, type = "Ljung-Box")

#check result
coeftest(model_urgent)

#### Step 6: Time series: New Zealand ####

##### 6.1 Prepare time series #####

#aggregate use of urgent labels across outlets
ts_newzealand <- data %>%
  
  #filter forNew Zealand coverage
  filter(country == "New Zealand") %>%
  
  #group by date
  group_by(date) %>%
  
  #create relative share of coverage with urgent labels
  summarize(urgent_binary = sum(urgent_binary)/n()*100) %>%
  ungroup() %>%
  
  #sort by date for extraction of time series
  arrange(date) 

##### 6.2 Check assumptions & visualize #####

#check
ggtsdisplay(ts_newzealand$urgent_binary, plot.type = "partial", 
            points = FALSE, lag.max = 250, smooth = TRUE,
            main = "Use of Urgent Labels across Time",
            xlab = "Units of Time (Months)",
            ylab = "% of articles using labels",
            theme = theme_hc())

#transform to time series format: 12 months, starting in 2006
x <- ts(ts_newzealand$urgent_binary, frequency = 12, start = c(2006, 1))

#inspect shift after Guardian policy virtually, date of implementation: May 2019
#source: https://www.theguardian.com/environment/2019/may/17/why-the-guardian-is-changing-the-language-it-uses-about-the-environment)
plot(x)
abline(v=2019, col = "gray", lty = "dashed", lwd=2)

#control variable 1: NZ declares "climate emergency" (regional & national effects)
#source: https://en.wikipedia.org/wiki/Climate_emergency_declarations_in_New_Zealand
nz_declaration <- as.numeric(as.yearmon(time(x)) %in% c("Mai 2019", "Jun 2019", "Aug 2019", "Dez 2020"))

##### 6.3 Model time series #####

# Use automated approach to identify parameters
model_urgent <- auto.arima(x, 
                           xreg = cbind(guardian_edit, increase, nz_declaration),
                           stepwise = FALSE,
                           trace = TRUE)

# Check residuals
checkresiduals(model_urgent)

#no autocorrelation in residuals?
Box.test(model_urgent$residuals, lag = 24, type = "Ljung-Box")

#check result
coeftest(model_urgent)

#### Step 7: Time series: South Africa ####

##### 7.1 Prepare time series #####

#aggregate use of urgent labels across outlets
ts_southafrica <- data %>%
  
  #filter for South African coverage
  filter(country == "South Africa") %>%
  
  #group by date
  group_by(date) %>%
  
  #create relative share of coverage with urgent labels
  summarize(urgent_binary = sum(urgent_binary)/n()*100) %>%
  ungroup() %>%
  
  #sort by date for extraction of time series
  arrange(date) 

#add missing values (for months without coverage)
ts_southafrica <- rbind(ts_southafrica, data %>%
                        filter(date %!in% ts_southafrica$date) %>%
                        pull("date") %>%
                        unique() %>%
                        data.frame("date" = zoo::as.yearmon(.),
                                   "urgent_binary" = NA) %>%
                        select("date", "urgent_binary")) %>%
  arrange(date)

##### 7.2 Check assumptions & visualize #####

#check
ggtsdisplay(ts_southafrica$urgent_binary, plot.type = "partial", 
            points = FALSE, lag.max = 250, smooth = TRUE,
            main = "Use of Urgent Labels across Time",
            xlab = "Units of Time (Months)",
            ylab = "% of articles using labels",
            theme = theme_hc())

#transform to time series format: 12 months, starting in 2006
x <- ts(ts_southafrica$urgent_binary, frequency = 12, start = c(2006, 1))

#inspect shift after Guardian policy virtually, date of implementation: May 2019
#source: https://www.theguardian.com/environment/2019/may/17/why-the-guardian-is-changing-the-language-it-uses-about-the-environment)
plot(x)
abline(v=2019, col = "gray", lty = "dashed", lwd=2)

##### 7.3 Model time series #####

# Use automated approach to identify parameters
model_urgent <- auto.arima(x, 
                           xreg = cbind(guardian_edit, increase),
                           stepwise = FALSE,
                           trace = TRUE)

# Check residuals
checkresiduals(model_urgent)

#no autocorrelation in residuals?
Box.test(model_urgent$residuals, lag = 24, type = "Ljung-Box")

#check result
coeftest(model_urgent)

#### Step 8: Time series: Thailand ####

##### 8.1 Prepare time series #####

#aggregate use of urgent labels across outlets
ts_thailand <- data %>%
  
  #filter for Thailandish coverage
  filter(country == "Thailand") %>%
  
  #group by date
  group_by(date) %>%
  
  #create relative share of coverage with urgent labels
  summarize(urgent_binary = sum(urgent_binary)/n()*100) %>%
  ungroup() %>%
  
  #sort by date for extraction of time series
  arrange(date) 

##### 8.2 Check assumptions & visualize #####

#check
ggtsdisplay(ts_thailand$urgent_binary, plot.type = "partial", 
            points = FALSE, lag.max = 250, smooth = TRUE,
            main = "Use of Urgent Labels across Time",
            xlab = "Units of Time (Months)",
            ylab = "% of articles using labels",
            theme = theme_hc())

#transform to time series format: 12 months, starting in 2006
x <- ts(ts_thailand$urgent_binary, frequency = 12, start = c(2006, 1))

#inspect shift after Guardian policy virtually, date of implementation: May 2019
#source: https://www.theguardian.com/environment/2019/may/17/why-the-guardian-is-changing-the-language-it-uses-about-the-environment)
plot(x)
abline(v=2019, col = "gray", lty = "dashed", lwd=2)

##### 8.3 Model time series #####

# Use automated approach to identify parameters
model_urgent <- auto.arima(x, 
                           xreg = cbind(guardian_edit, increase),
                           stepwise = FALSE,
                           trace = TRUE)

# Check residuals
checkresiduals(model_urgent)

#no autocorrelation in residuals?
Box.test(model_urgent$residuals, lag = 24, type = "Ljung-Box")

#check result
coeftest(model_urgent)

#### Step 9: Time series: UK ####

##### 9.1 Prepare time series #####

#aggregate use of urgent labels across outlets
ts_uk <- data %>%
  
  #filter for UK coverage
  filter(country == "UK") %>%
  
  #group by date
  group_by(date) %>%
  
  #create relative share of coverage with urgent labels
  summarize(urgent_binary = sum(urgent_binary)/n()*100) %>%
  ungroup() %>%
  
  #sort by date for extraction of time series
  arrange(date) 

##### 9.2 Check assumptions & visualize #####

#check
ggtsdisplay(ts_uk$urgent_binary, plot.type = "partial", 
            points = FALSE, lag.max = 250, smooth = TRUE,
            main = "Use of Urgent Labels across Time",
            xlab = "Units of Time (Months)",
            ylab = "% of articles using labels",
            theme = theme_hc())

#transform to time series format: 12 months, starting in 2006
x <- ts(ts_uk$urgent_binary, frequency = 12, start = c(2006, 1))

#inspect shift after Guardian policy virtually, date of implementation: May 2019
#source: https://www.theguardian.com/environment/2019/may/17/why-the-guardian-is-changing-the-language-it-uses-about-the-environment)
plot(x)
abline(v=2019, col = "gray", lty = "dashed", lwd=2)

##### 9.3 Model time series #####

# Use automated approach to identify parameters
model_urgent <- auto.arima(x, 
                           xreg = cbind(guardian_edit, increase),
                           stepwise = FALSE,
                           trace = TRUE)

# Check residuals
checkresiduals(model_urgent)

#no autocorrelation in residuals?
Box.test(model_urgent$residuals, lag = 24, type = "Ljung-Box")

#check result
coeftest(model_urgent)

#### Step 10: Time series: US ####

##### 10.1 Prepare time series #####

#aggregate use of urgent labels across outlets
ts_us <- data %>%
  
  #filter for US coverage
  filter(country == "USA") %>%
  
  #group by date
  group_by(date) %>%
  
  #create relative share of coverage with urgent labels
  summarize(urgent_binary = sum(urgent_binary)/n()*100) %>%
  ungroup() %>%
  
  #sort by date for extraction of time series
  arrange(date) 

##### 10.2 Check assumptions & visualize #####

#check
ggtsdisplay(ts_us$urgent_binary, plot.type = "partial", 
            points = FALSE, lag.max = 250, smooth = TRUE,
            main = "Use of Urgent Labels across Time",
            xlab = "Units of Time (Months)",
            ylab = "% of articles using labels",
            theme = theme_hc())

#transform to time series format: 12 months, starting in 2006
x <- ts(ts_us$urgent_binary, frequency = 12, start = c(2006, 1))

#inspect shift after Guardian policy virtually, date of implementation: May 2019
#source: https://www.theguardian.com/environment/2019/may/17/why-the-guardian-is-changing-the-language-it-uses-about-the-environment)
plot(x)
abline(v=2019, col = "gray", lty = "dashed", lwd=2)

##### 10.3 Model time series #####

# Use automated approach to identify parameters
model_urgent <- auto.arima(x, 
                           xreg = cbind(guardian_edit, increase),
                           stepwise = FALSE,
                           trace = TRUE)

# Check residuals
checkresiduals(model_urgent)

#no autocorrelation in residuals?
Box.test(model_urgent$residuals, lag = 24, type = "Ljung-Box")

#check result
coeftest(model_urgent)

#### Step 11: Stores results ####

#clean
rm(x)

#save
save.image(here("data", paste0("RQ2.RDATA")))