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
  filter(nature_of_incident == "BREATHING PROBLEMS") %>% 
  select(-nature_of_incident)
breatjing_problem_variables <- breatjing_problem_ts %>% mutate(t =row_number(), 
                                f1_sin = sin((2*pi*t)/7), f1_cos = cos((2*pi*t)/7),
                                f2_sin = sin((2*2*pi*t)/7), f2_cos = cos((2*2*pi*t)/7),
                                f3_sin = sin((3*2*pi*t)/7), f3_cos = cos((3*2*pi*t)/7),
                                f4_sin = sin((4*2*pi*t)/7), f4_cos = cos((4*2*pi*t)/7),
                                f5_sin = sin((5*2*pi*t)/7), f5_cos = cos((5*2*pi*t)/7),
                                f6_sin = sin((6*2*pi*t)/7), f6_cos = cos((6*2*pi*t)/7),
                                f7_sin = sin((7*pi*t)/7), f7_cos = cos((7*2*pi*t)/7))

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

predict(breatjing_problem_fit, n.ahead = 7, level = 0.9, global = TRUE,
        newxreg = breatjing_problem_new_data)
  


#using glm

reg_7day <-  glm(incidents ~ t +
               f1_sin + f1_cos +
               f2_sin + f3_cos +
               f3_sin + f3_cos +
               f4_sin + f4_cos +
               f5_sin + f5_cos +
               f6_sin + f6_cos +
               f7_sin + f7_cos ,
             data=breatjing_problem_train,family=poisson(link='log'))
predict(reg_7day, newdata = breatjing_problem_new_data, type = "response")   


# Using MASS

glmmalf<-glmmPQL(incidents ~ t +
                   f1_sin + f1_cos +
                   f2_sin + f3_cos +
                   f3_sin + f3_cos +
                   f4_sin + f4_cos +
                   f5_sin + f5_cos +
                   f6_sin + f6_cos +
                   f7_sin + f7_cos ,
                 random=~1|t,
                 correlation=corARMA(),
                 family=poisson(link="log"), 
                 data=breatjing_problem_train)

#is there a harmonic() function

library(nlme) # will be loaded automatically if omitted
summary(glmmPQL(y ~ trt + I(week > 2), random = ~ 1,
                family = binomial, data = bacteria))

# lme() in gamlss