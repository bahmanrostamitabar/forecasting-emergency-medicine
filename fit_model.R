#Resource
#https://cran.r-project.org/web/views/TimeSeries.html
#check: https://bbolker.github.io/mixedmodels-misc/glmmFAQ.html
#tscount: https://cran.r-project.org/web/packages/tscount/vignettes/tsglm.pdf
#https://pipiras.sites.oasis.unc.edu/timeseries/Nonlinear_2_-_Count_time_series_-_Menu.html
#https://hidda.github.io/forecasting/articles/extra/CHILI_tscount.html
#https://www.youtube.com/watch?v=6mIUmAUj0I0
#https://masalmon.eu/2017/02/12/wikideaths1_ts/
#file:///Users/bahmanrostami-tabar/Downloads/2015_31_report.pdf

library(tscount)
library(tidyverse)
library(tsibble)
library(fable)
library(lubridate)
library(MASS)

# Read data
incidents_original <- readxl::read_excel("data/Nature_of_Incidents_Attended.xlsx") %>%
  mutate(date = as_date(Incident_Date)) %>%
  janitor::clean_names() %>%
  force_tz(date, tz = "GB") 
incidents <- incidents_original %>%   mutate_at(.vars = "nature_of_incident", .funs=as.numeric) %>% 
  mutate(nature_of_incident= ifelse(!is.na(nature_of_incident),nature_of_incident_description , "other")) %>% 
  group_by(lhb_code, category, nature_of_incident, date) %>% 
  summarise(incidents = sum(total_incidents)) %>% ungroup() %>% 
  mutate(category = factor(category, level=c("RED","AMBER","GREEN"))) %>% 
  as_tsibble(index = date, key = c(lhb_code, category, nature_of_incident)) %>%
  fill_gaps(incidents = 0)



# prepare data
breatjing_problem_ts <- incidents %>% index_by(date) %>% 
  group_by(nature_of_incident) %>% 
  summarise(incidents = sum(incidents)) %>% 
  filter(nature_of_incident == "BREATHING PROBLEMS") %>%  dplyr::select(-nature_of_incident)

ggplot(breatjing_problem_ts, aes(x = incidents)) + 
  geom_histogram(binwidth = 1, boundary = 0, color = "white")

breatjing_problem_variables <- breatjing_problem_ts %>% mutate(t =row_number(), 
                                f1_sin = sin((2*pi*t)/7), f1_cos = cos((2*pi*t)/7),
                                f2_sin = sin((2*2*pi*t)/7), f2_cos = cos((2*2*pi*t)/7),
                                f3_sin = sin((3*2*pi*t)/7), f3_cos = cos((3*2*pi*t)/7),
                                holiday= if_else(incidents > 150, 1,0))
View(breatjing_problem_variables)
breatjing_problem_train <-breatjing_problem_variables %>% filter_index(~ "2019-07-24")
breatjing_problem_test <-breatjing_problem_variables %>% filter_index("2019-07-25" ~ .)
breatjing_problem_new_data <- breatjing_problem_test %>% as_tibble() %>% select(-date, -incidents)

breatjing_problem_train_predictor <- breatjing_problem_train %>% as_tibble() %>% 
  select(-date, -incidents)
breatjing_problem_train_ts <- ts(breatjing_problem_train$incidents, frequency = 365, start = decimal_date(as.Date("2015-10-02")))

# using tsglm in tscount
breatjing_problem_fit <- tsglm(breatjing_problem_train_ts,
                      model = list(past_obs = 1), link = "log", distr = "poisson",
                      xreg = breatjing_problem_train_predictor)
summary(breatjing_problem_fit)
predict(breatjing_problem_fit, n.ahead = 7, level = 0.9, global = TRUE,
        newxreg = breatjing_problem_new_data,method = "bootstrap", B = 1000)
  


#using glm

reg_7day <-  glm(incidents ~ t +
               f1_sin + f1_cos +
               f2_sin + f3_cos +
               f3_sin + f3_cos ,
             data=breatjing_problem_train,family=poisson(link='log'))
predict(reg_7day, newdata = breatjing_problem_new_data, type = "response")   


# Using MASS

glmmalf<-glmmPQL(incidents ~ t +
                   f1_sin + f1_cos +
                   f2_sin + f3_cos +
                   f3_sin + f3_cos ,
                 random=~ 1 | holiday,
                 correlation=corARMA(form = ~ 1 | holiday, p = 1, q = 1),
                 family=poisson(link="log"), 
                 data=breatjing_problem_train)
predict(glmmalf, newdata = breatjing_problem_new_data)   

# in random , we can put nature of incidents or category as random effect
# we can use trend and fourier terms for the fixed efefct
#is there a harmonic() function

library(forecast)
h <- 7
testSet <- 7

obs <- length(breatjing_problem_ts)
xFourier <- fourier(msts(as.vector(breatjing_problem_ts),seasonal.periods=c(7,365.25)), K=c(3,10))

breatjing_problem_variables <- breatjing_problem_ts %>% mutate(t =row_number(), 
                                holiday= if_else(incidents > 150, 1,0)) %>% bind_cols(as_tibble(xFourier))

breatjing_problem_train <-breatjing_problem_variables %>% filter_index(~ "2019-07-24") %>% select(-date)
breatjing_problem_test <-breatjing_problem_variables %>% filter_index("2019-07-25" ~ .)
breatjing_problem_new_data <- breatjing_problem_test %>% as_tibble() %>% select(-date, -incidents)

regressionModel <- alm(incidents~., data=breatjing_problem_train,
                       distribution="dpois", ar=1,
                       maxeval=10000, ftol_rel=1e-10)
summary(regressionModel)
Acf(regressionModel$residuals)
# nsim is needed just in case, if everything fails and bootstrap is used
testForecast <- predict(regressionModel,newdata =breatjing_problem_test)

## brm
#https://thestudyofthehousehold.netlify.app/2018/02/13/2018-02-13-easily-made-fitted-and-predicted-values-made-easy/
#https://paul-buerkner.github.io/blog/brms-blogposts/
library(brms)
data("inholidayer", package = "brms")
head(inholidayer, n = 1)
fit3 <- brm(formula = rating ~ treat + period + carry + (1 | subject), 
            data = inholidayer, family = cumulative)
### fit a stopping ratio model with equidistant thresholds 
### and category specific effects
fit4 <- brm(formula = rating ~ period + carry + cse(treat) + (1 | subject),
            data = inholidayer, family = sratio(threshold = "equidistant"),
            prior = set_prior("normal(-1,2)", coef = "treat"))

### obtain model summaries and plots
summary(fit4, waic = TRUE)
plot(fit4, ask = FALSE)
