#### Do time series for RQ2 ####
library("here")
library("dplyr")
library("lubridate")
library("xts")
library("forecast")
library("ggplot2")
library("ggthemes")
library("lmtest")

#for further tutorials, see e.g.:
#https://nicolarighetti.github.io/Time-Series-Analysis-With-R/intervention-analysis.html
#https://bmcmedresmethodol.biomedcentral.com/articles/10.1186/s12874-021-01235-8#Sec2 & R Code
#https://www.r-bloggers.com/2017/10/arima-models-and-intervention-analysis/
#example: https://varollab.com/data/publications/files/2018-emotions.pdf
#https://projecteuclid.org/journals/annals-of-applied-statistics/volume-9/issue-1/Inferring-causal-impact-using-Bayesian-structural-time-series-models/10.1214/14-AOAS788.full
#https://academic.oup.com/ije/article/46/1/348/2622842?login=false
#https://books.google.de/books?hl=de&lr=&id=bACvDwAAQBAJ&oi=fnd&pg=PP1&dq=%22interrupted+time+series%22&ots=BJTfPPcjSe&sig=_21X0Yw4dJNLYb84drIUeJ0u5Xo#v=onepage&q=%22interrupted%20time%20series%22&f=false
#https://www.bmj.com/content/350/bmj.h2750.abstract
#https://journals.sagepub.com/doi/abs/10.1177/1536867X1501500208
#Kowi 0: https://www.tandfonline.com/doi/full/10.1080/19312450902809672
#Kowi 1: https://www.tandfonline.com/doi/full/10.1080/10810730.2018.1528318
#Kowi 2: https://www.degruyter.com/document/doi/10.1515/omgc-2023-0012/html
#Kowi 3: https://academic.oup.com/joc/article/73/1/73/6849391#393973361



#### Step 1: Load data
data <- read.csv(here("data", paste0("output.csv"))) %>%
  select("outlet", "year", "month", "neutral_binary", "urgent_binary")

#### Step 2: Prepare data

#transform date variable
data <- data %>%
  mutate(month = paste0(0, month),
         month = replace(month, month == "010", "10"),
         month = replace(month, month == "011", "11"),
         month = replace(month, month == "012", "12"),
         date = paste0(year, "/", month, "/", "1"),
         date = as.Date(date, format = "%Y/%m/%d"),
         date = zoo::as.yearmon(date))

#aggregate use of urgent and neutral labels across outlets
ts_aggregated <- data %>%
  group_by(date) %>%
  summarize(urgent_binary = sum(urgent_binary)/n()*100,
            neutral_binary = sum(neutral_binary)/n()*100) %>%
  ungroup() %>%
  #sort by date for extraction of time series
  arrange(date) 

#### Step 3: Check assumptions
ggtsdisplay(ts_aggregated$urgent_binary, plot.type = "partial", 
            points = FALSE, lag.max = 250, smooth = TRUE,
            main = "Use of Urgent Labels across Time",
            xlab = "Units of Time (Months)",
            ylab = "% of articles using labels",
            theme = theme_hc())

#transform to time series: two ts, each for urgent vs. neutral terms
ts_urgent <- ts(ts_aggregated$urgent_binary, frequency = 12, start = c(2006, 1))
ts_neutral <- ts(ts_aggregated$neutral_binary, frequency = 12, start = c(2006, 1))

#inspect shifts virtually, with Guardian change as measure (in May 2029, see: https://www.theguardian.com/environment/2019/may/17/why-the-guardian-is-changing-the-language-it-uses-about-the-environment)
plot(ts_urgent)
abline(v=2019, col = "gray", lty = "dashed", lwd=2)

plot(ts_neutral)
abline(v=2019, col = "gray", lty = "dashed", lwd=2)

#independent variable 1: step change or level shift (i.e., shift of mean after May 2019)
guardian_edit <- as.numeric(as.yearmon(time(ts_urgent)) >= "Mai 2019")

#independent variable 2: ramp (i.e., linear increase after May 2019)
increase <- c(rep(0, 160), seq(1, 204-160, 1))

#### Step 3: Test model

# Use automated approach to identify parameters
model_urgent <- auto.arima(ts_urgent, xreg = cbind(guardian_edit, increase), stepwise = FALSE)

# Check residuals
checkresiduals(model_urgent)

#test for heteroscedasticiy using the Lagrange Multiplier test, needs to be n.s.
test <- arima(x,c(0,1,1),xreg=iv) 
test <- aTSA::arch.test(test)
paste0(round(test[3,4],3), ", ",round(test[3,5],3))

#check result
model_urgent
coeftest(model_urgent)
