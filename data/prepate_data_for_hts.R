library(glue)
library(forecast)
library(hts)

library(tidyverse)
library(tsibble)
library(fable)
library(fabletools)
library(feasts)
library(lubridate)
library(fable.tscount)
library(tscount)

# import data
f_horizon <- 5*7
# Read data
incidents_original <- readxl::read_excel("data/Nature_of_Incidents_Attended.xlsx") %>%
  mutate(date = as_date(Incident_Date)) %>%
  janitor::clean_names() %>%
  force_tz(date, tz = "GB") 
# nature of incidents with low volume
nature_of_incident_low <- c("AUTOMATIC CRASH NOTIFICATION",
                            "INACCESSIBLE INCIDENT/OTHER ENTRAP",
                            "INTERFACILITY EVALUATION/TRANSFER",
                            "MAJOR INCIDENT - OVERRIDE PROQA",
                            "TRANSFER/INTERFACILITY/PALLIATIVE")

# read holiday data
holiday <- readxl::read_excel("data/holiday_rugby.xlsx") %>% 
  select(date,public_holiday,school_holiday) %>% 
  mutate(date=as_date(date)) %>% 
  mutate(public_holiday_d=if_else(is.na(public_holiday), 0,1),
         school_holiday_d=if_else(is.na(school_holiday), 0,1)) %>% 
  mutate(xmas=if_else(public_holiday=="Christmas Day","1","0"),
         new_years_day=if_else(public_holiday=="New Years Day",1,0)) %>% 
  mutate_at(vars(xmas,new_years_day), ~replace(.,is.na(.),0)) 

# create dummies for holidays
holiday_dummy <- holiday %>% select(date,
                                    public_holiday_d,
                                    school_holiday_d,
                                    xmas,
                                    new_years_day) %>% 
  mutate_at(vars(public_holiday_d,school_holiday_d,xmas,new_years_day),
            ~as_factor(.)) %>% 
  as_tsibble(index = date)
holiday_school <- holiday_dummy %>% select(date,school_holiday_d) %>%
  filter(school_holiday_d==1) %>% 
  mutate(holiday = "school_holiday") %>% select(-school_holiday_d)
holiday_public <- holiday_dummy %>% select(date,public_holiday_d) %>%
  filter(public_holiday_d==1) %>% 
  mutate(holiday = "public_holiday") %>% select(-public_holiday_d)

holiday_xmas <- holiday_dummy %>% select(date,xmas) %>%
  filter(xmas==1) %>% 
  mutate(holiday = "xmas") %>% select(-xmas)

holiday_newyear <- holiday_dummy %>% select(date,new_years_day) %>%
  filter(new_years_day==1) %>% 
  mutate(holiday = "new_years_day") %>% select(-new_years_day)

school_holiday <- data_frame(
  holiday = 'school_holiday',
  ds = holiday_school$date,
  lower_window = -1,
  upper_window = 1
)

public_holiday <- data_frame(
  holiday = 'public_holiday',
  ds = holiday_public$date,
  lower_window = -1,
  upper_window = 1
)

xmas_holiday <- data_frame(
  holiday = 'xmas',
  ds = holiday_xmas$date,
  lower_window = -1,
  upper_window = 1
)

new_years_day_holiday <- data_frame(
  holiday = 'new_years_day',
  ds = holiday_newyear$date,
  lower_window = -1,
  upper_window = 1
)

holiday_prophet <- school_holiday %>% 
  bind_rows(public_holiday) %>%
  bind_rows(xmas_holiday) %>%
  bind_rows(new_years_day_holiday)

write_rds(holiday_dummy,"data/holiday_dummy.rds")
# count number of incidents
incidents <- incidents_original %>%   mutate_at(.vars = "nature_of_incident", .funs=as.numeric) %>% 
  mutate(nature_of_incident= ifelse(!is.na(nature_of_incident),nature_of_incident_description , "upgrade")) %>% 
  mutate(nature_of_incident= ifelse(nature_of_incident %in% nature_of_incident_low,"other" , nature_of_incident)) %>% 
  group_by(lhb_code, category, nature_of_incident, date) %>% 
  summarise(incidents = sum(total_incidents),.groups = "drop") %>% 
  mutate(category = factor(category, level=c("RED","AMBER","GREEN")),
         nature_of_incident = factor(nature_of_incident),
         lhb_code = factor(lhb_code))

incidents_tsbl <- incidents %>%  
  as_tsibble(index = date, key = c(lhb_code, category, nature_of_incident)) %>%
  fill_gaps(incidents = 0, .full = TRUE)
write_rds(incidents_tsbl,"data/incidents_tsbl.rds")

# prepare data for hts format
incidents_tsbl <- read_rds("data/incidents_tsbl.rds")

incident_modify <- incidents_tsbl %>% 
  mutate(nature = case_when(
    nature_of_incident == "ABDOMINAL PAIN/PROBLEMS" ~ "ABDOMINAL",
    nature_of_incident == "ALLERGIES(REACTIONS)/ENVENOMATIONS" ~ "ALLERGIES",
    nature_of_incident == "ANIMAL BITES/ATTACKS" ~ "ANIMALBIT",
    nature_of_incident == "ASSAULT/SEXUAL ASSAULT" ~ "SEXASSAUL",
    nature_of_incident == "BREATHING PROBLEMS" ~ "BREATHING",
    nature_of_incident == "BURNS(SCALDS)/EXPLOSION" ~ "BURNSSCAL",
    nature_of_incident == "CARBON MONOXIDE/INHALATION/HAZCHEM" ~ "CARBONMON",
    nature_of_incident == "CARDIAC/RESPIRATORY ARREST/DEATH" ~ "RESPIRATO",
    nature_of_incident == "CHEST PAIN" ~ "CHESTPAIN",
    nature_of_incident == "CONVULSIONS/FITTING" ~ "CONVULSIO",
    nature_of_incident == "DIABETIC PROBLEMS" ~ "DIABETICS",
    nature_of_incident == "DROWNING(NEAR)/DIVING/SCUBA" ~ "DROWNINGS",
    nature_of_incident == "ELECTROCUTION/LIGHTNING" ~ "LIGHTNING",
    nature_of_incident == "HAEMORRHAGE/LACERATIONS" ~ "HAEMORRHA",
    nature_of_incident == "HEALTH CARE PROFESSIONAL" ~ "PROFESSIO",
    nature_of_incident == "OVERDOSE/POISONING (INGESTION)" ~ "OVERDOSES",
    nature_of_incident == "PREGNANCY/CHILDBIRTH/MISCARRIAGE" ~ "PREGNANCY",
    nature_of_incident == "PROQA COMPLETED ON CARDSET" ~ "PROQACOMP",
    nature_of_incident == "PSYCH/ABNORMAL BEHAVIOUR/SUICIDE" ~ "SUICIDEPS",
    nature_of_incident == "SICK PERSON - SPECIFIC DIAGNOSIS" ~ "SICKPERSO",
    nature_of_incident == "STAB/GUNSHOT/PENTRATING TRAUMA" ~ "STABGUNSH",
    nature_of_incident == "STROKE - CVA" ~ "STROKECVA",
    nature_of_incident == "TRAFFIC/TRANSPORTATION ACCIDENTS" ~ "TRAFFICAC",
    nature_of_incident == "TRAUMATIC INJURIES, SPECIFIC" ~ "TRAUMATIC",
    nature_of_incident == "UNCONSCIOUS/FAINTING(NEAR)" ~ "UNCONSCIO",
    nature_of_incident == "UNKNOWN PROBLEM - COLLAPSE-3RD PTY" ~ "UNKNOWNPR",
    nature_of_incident == "BACK PAIN (NON-TRAUMA/NON-RECENT)" ~ "BACKPAINS",
    nature_of_incident == "EYE PROBLEMS/INJURIES" ~ "EYEPROBLE",
    nature_of_incident == "HEART PROBLEMS/A.I.C.D" ~ "HEARTPROB",
    nature_of_incident == "HEAT/COLD EXPOSURE" ~ "HEATCOLDS",
    nature_of_incident == "CHOKING" ~ "CHOKINGSS",
    nature_of_incident == "FALLS" ~ "FALLSTHRO",
    nature_of_incident == "HEADACHE" ~ "HEADACHES",
    nature_of_incident == "upgrade" ~ "upgradess",
    nature_of_incident == "other" ~ "allothers"
  ))

incident_ready <- incident_modify %>% as_tibble() %>% 
  select(date,lhb=lhb_code, category, nature, incident=incidents) %>% 
  mutate(category = case_when(category =="RED" ~ "RED",
                              category =="GREEN" ~ "GRE",
                              category =="AMBER" ~ "AMB")) %>% 
  mutate(lhb =case_when(lhb =="CTM" ~ "CT",
                        lhb =="POW" ~ "PO",
                        lhb =="AB" ~ "AB",
                        lhb =="BC" ~ "BC",
                        lhb =="CV" ~ "CV",
                        lhb =="HD" ~ "HD",
                        lhb =="SB" ~ "SB")) %>%
  mutate(region = case_when(
    lhb == "CT" ~ "S",
    lhb == "PO" ~ "C",
    lhb == "AB" ~ "S",
    lhb == "BC" ~ "N",
    lhb == "CV" ~ "S",
    lhb == "HD" ~ "C",
    lhb == "SB" ~ "C"
  ))

a1 <- incident_ready %>% 
  filter(lhb=="CV", category=="RED" , nature=="FALLSTHRO") %>% 
  select(date,delet=incident)
incident_gts <- a1
#incident_gts <- incident_gts %>% bind_cols(a)
#incident_gts <- incident_gts %>% select(c(1,2))
hb <- c("AB",  "BC",  "CT", "CV",  "HD",  "PO", "SB")
cat <- c("RED", "AMB", "GRE")
nat <- c("ABDOMINAL","ALLERGIES","ANIMALBIT","SEXASSAUL",
         "BREATHING", "BURNSSCAL", "CARBONMON", "RESPIRATO",
         "CHESTPAIN", "CHOKINGSS", "CONVULSIO", "DIABETICS",
         "DROWNINGS", "LIGHTNING", "FALLSTHRO", "HAEMORRHA",
         "HEADACHES", "PROFESSIO", "OVERDOSES", "PREGNANCY",
         "PROQACOMP", "SUICIDEPS", "SICKPERSO", "STABGUNSH",
         "STROKECVA", "TRAFFICAC", "TRAUMATIC", "UNCONSCIO",
         "UNKNOWNPR", "upgradess", "BACKPAINS", "EYEPROBLE",
         "HEARTPROB", "HEATCOLDS", "allothers")
region <- c("C","N","S")
for (i in (1:length(hb))) {
  region = 
    case_when(
      hb[i] == "CT" ~ "S",
      hb[i] == "PO" ~ "C",
      hb[i] == "AB" ~ "S",
      hb[i] == "BC" ~ "N",
      hb[i] == "CV" ~ "S",
      hb[i] == "HD" ~ "C",
      hb[i] == "SB" ~ "C"
    )
  for (j in (1:length(cat))) {
    for (k in (1:length(nat))) {
      
      fil <- incident_ready %>% 
        filter(lhb==hb[i], 
               category==cat[j] , 
               nature==nat[k])
      name <- glue(paste0(region, 
                          hb[i],
                          cat[j],
                          nat[k]))
      if (nrow(fil) > 0) {
        a <- fil %>% select(incident) 
        colnames(a) <- name
        incident_gts <- incident_gts %>% bind_cols(a)
      } 
      #      else{
      #        a <- as_tibble(matrix(0, 1400,1)) 
      #        colnames(a) <- name
      #        incident_gts <- incident_gts %>% 
      #          bind_cols(a)
      #      }
    }
  }
}


incident_all <- incident_gts %>% 
  select(-delet,-date)
write_rds(incident_all,"data/incident_all.rds")
