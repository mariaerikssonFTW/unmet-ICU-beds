library(tidyverse)
library(dplyr)
library(tableone)
library(lubridate)
library(noacsr)
library(glm2)
library(ggplot2)
library(pROC)

#setwd("C:/Users/maria/Downloads/Packt Learning RStudio for R Statistical Computing 2012 RETAIL eBook-repackb00k/unmet-ICU-beds")
source("johanna/functions.R")

# 1. Import the data
#
# Read the dataset and codebook from github
# data <- import_csv_from_github("https://github.com/titco/titco-I/blob/master/titco-I-full-dataset-v1.csv")
# #data <- import_csv_from_github("https://github.com/titco/titco-I/blob/master/titco-I-limited-dataset-v1.csv")
# codebook <- import_csv_from_github("https://github.com/titco/titco-I/blob/master/codebook-titco-I-v1.csv")


# #för jag vill kunna köra offline
# save(data, file = "data.RData")
# save(codebook, file = "codebook.RData")

load("data.RData")
load("codebook.RData")

# unmet_function <- function(formula, data, indices) {
#   d <- data[indices,] #allows boot to select sample


outcome=data %>%
  select(doi, toi, doa, toa, doar, toar,  dom_1, tom_1,dom_2, tom_2,dodd, todd,tran,spo2_o2_1) %>%
  # Combine date and time to make a datetime object
  mutate(injury_time = as_datetime(paste0(doi, toi))) %>%
  mutate(arrival_time = as_datetime(paste0(doa, toa))) %>%
  mutate(admission_time = as_datetime(paste0(doa, toa))) %>%
  mutate(firstsurvey_time = as_datetime(paste0(dom_1, tom_1))) %>%
  mutate(secondsurvey_time = as_datetime(paste0(dom_2, tom_2))) %>%
  mutate(time_of_death = as_datetime(paste0(dodd, todd))) %>%
  # Calculate difference in hours between arrival and death
  mutate(time_to_death = round(as.numeric(time_of_death - arrival_time, units = "hours"),0))%>%
  
  #här börjar min påbyggnad
  mutate(time_to_first = round(as.numeric(firstsurvey_time-arrival_time , units = "hours"),2))%>%
  mutate(time_between_surveys = round(as.numeric(secondsurvey_time - firstsurvey_time, units = "hours"),2)) %>%
  #vi tar dessa åt sidan, för vi vill bara göra detta på de som inte är transfererade, eftersom de snurrat förbi ett annat sjukhus blir tiden från injury till arrival missvisande
  # approach 1, blev kortare
  # filter(tran %in% c("No"))%>%
  #   mutate(injury_time_direct = as_datetime(paste0(doi, toi)))  %>%
  #   mutate(arrival_time_direct = as_datetime(paste0(doar, toar))) %>%
  #   mutate(firstsurvey_time_direct = as_datetime(paste0(dom_1, tom_1)))
  mutate(stillalive_6h = case_when(time_to_death > 6 ~ 1,
                                   time_to_death < 6 ~ 0)) %>%
  #återkom fatta hur du filtrerar på detta
  mutate(delay2 = round(as.numeric(arrival_time-injury_time , units = "hours"),0))%>%
  mutate(delay23 = round(as.numeric(firstsurvey_time-injury_time , units = "hours"),0))%>%
  mutate(delay2ifdirect = case_when(str_detect(tran, "No") & delay2<72 ~ delay2)) # %>%

#addera kolumnerna
data=data.frame( data, outcome)

#lite rensning
# Updates to the dataset
data <- data %>%
  # Convert age to numeric
  mutate(age = parse_number(age)) %>%
  #applicera överenskomna filtreringar
  #  filter(data$stillalive==1)%>%
  #... återkom
  
  mutate(rts_sbp1 = case_when(sbp_1 ==0 & hr_1 <1 & sbp_1 !='NA'  ~ 0,
                              sbp_1 < 49 & sbp_1 !='NA' ~ 1,
                              sbp_1 < 75 & sbp_1 !='NA' ~ 2,
                              sbp_1 < 89 & sbp_1 !='NA' ~ 3,
                              sbp_1 < 300 & sbp_1 !='NA' ~4)) %>%
  
  mutate(rts_sbp2 = case_when(sbp_2 ==0 & hr_2<1  ~ 0,
                              sbp_2 < 49  & sbp_2 !='NA' ~ 1,
                              sbp_2 < 75  & sbp_2 !='NA' ~ 2,
                              sbp_2 < 89   & sbp_2 !='NA' ~ 3,
                              sbp_2 < 300 & sbp_2 !='NA' ~ 4)) %>%
  
  mutate(rts_rr1 = case_when(rr_1 ==0   ~ 0,
                             rr_1 < 5   ~ 1,
                             rr_1 < 9   ~ 2,
                             rr_1 > 29   ~ 3,
                             rr_1 > 9 & rr_1 < 29   ~ 4)) %>%
  
  mutate(rts_rr2 = case_when(rr_2 ==0   ~ 0,
                             rr_2 < 5   ~ 1,
                             rr_2 < 9   ~ 2,
                             rr_2 > 29   ~ 3,
                             rr_2 > 9 & rr_2 < 29 ~ 4)) %>%
  
  #får in transfer
  mutate(ambulancefromthescene = case_when(str_detect(tran, "No") & str_detect(mot, "Ambulance") ~ 1)) %>%
  
  mutate(tranclass= case_when(ambulancefromthescene==1   ~ 1,
                              str_detect(mot, "Police")  ~ 2,
                              ambulancefromthescene==0   ~ 3,
                              str_detect(mot, "Carried by man")   ~ 3,
                              str_detect(mot, "Other")   ~ 3,
                              str_detect(mot, "Private car")   ~ 3,
                              str_detect(mot, "Taxi, motor rickshaw")   ~ 3    )) %>%
  
  mutate(bl_rec= case_when(ubr_1>1   ~ "Yes",
                           ubr_1<1.5   ~ "No")) %>%
  
  mutate(sc_hi= case_when(sc>3   ~ "Yes",
                          sc<3.000001   ~ "No")) %>%
  
  # logical or för paste ||
  
  mutate(gcs_v_1_class= case_when(str_detect(gcs_v_1, "1")    ~ 1,
                                  str_detect(gcs_v_1, "1v")   ~ 2,
                                  str_detect(gcs_v_1, "2")    ~ 3,
                                  str_detect(gcs_v_1, "3" )   ~ 4,
                                  str_detect(gcs_v_1, "4")    ~ 4,
                                  str_detect(gcs_v_1, "5" )   ~ 4,
                                  str_detect(gcs_v_1, "6" )   ~ 4)) %>%
  
  mutate(gcs_m_1_class= case_when(str_detect(gcs_m_1, "1")    ~ 1,
                                  str_detect(gcs_m_1, "2")    ~ 3,
                                  str_detect(gcs_m_1, "3" )   ~ 4,
                                  str_detect(gcs_m_1, "4")    ~ 4,
                                  str_detect(gcs_m_1, "5" )   ~ 4,
                                  str_detect(gcs_m_1, "6" )   ~ 4)) %>%
  
  mutate(gcs_e_1_class= case_when(str_detect(gcs_e_1, "1")    ~ 1,
                                  str_detect(gcs_e_1, "1v")   ~ 2,
                                  str_detect(gcs_e_1, "2")    ~ 3,
                                  str_detect(gcs_e_1, "3" )   ~ 4,
                                  str_detect(gcs_e_1, "4")    ~ 4,
                                  str_detect(gcs_e_1, "5" )   ~ 4,
                                  str_detect(gcs_e_1, "6" )   ~ 4)) %>%
  
  #second survey. du ,åste pricka allt så du kan få NA
  mutate(gcs_v_2_class= case_when(str_detect(gcs_v_2, "1")    ~ 1,
                                  str_detect(gcs_v_2, "1v")   ~ 2,
                                  str_detect(gcs_v_2, "2")    ~ 3,
                                  str_detect(gcs_v_2, "3" )   ~ 4,
                                  str_detect(gcs_v_2, "4")    ~ 4,
                                  str_detect(gcs_v_2, "5" )   ~ 4,
                                  str_detect(gcs_v_2, "6" )   ~ 4)) %>%
  
  mutate(gcs_m_2_class= case_when(str_detect(gcs_m_2, "1")    ~ 1,
                                  str_detect(gcs_m_2, "2")    ~ 3,
                                  str_detect(gcs_m_2, "3" )   ~ 4,
                                  str_detect(gcs_m_2, "4")    ~ 4,
                                  str_detect(gcs_m_2, "5" )   ~ 4,
                                  str_detect(gcs_m_2, "6" )   ~ 4)) %>%
  
  mutate(gcs_e_2_class= case_when(str_detect(gcs_e_2, "1")    ~ 1,
                                  str_detect(gcs_e_2, "1v")   ~ 2,
                                  str_detect(gcs_e_2, "2")    ~ 3,
                                  str_detect(gcs_e_2, "3" )   ~ 4,
                                  str_detect(gcs_e_2, "4")    ~ 4,
                                  str_detect(gcs_e_2, "5" )   ~ 4,
                                  str_detect(gcs_e_2, "6" )   ~ 4)) %>%
  
  # Add id column to the dataset
  #  mutate(id = row_number()) %>% #jag byter ut, finns seqn redan
  # Add our outcome, if patient has a recorded length in the ICU then treated_icu = 1
  mutate(treated_icu = case_when(licu > 24 & hos ==6273 ~ 1,
                                 licu < 25 & hos ==6273 ~ 0,
                                 licu > 0 & hos ==7215 ~ 1,
                                 licu > 0 & hos ==7842 ~ 1,
                                 licu > 0 & hos ==8264 ~ 1,
                                 licu == 0 & hos ==7215 ~ 0,
                                 licu == 0 & hos ==7842 ~ 0,
                                 licu == 0 & hos ==8264 ~ 0)) %>% #var det inte det här vi sa?
  # mutate(treated_icu = case_when(licu > 24 ~ 1,
  #                                licu < 25 ~ 0))
  mutate(spO2_1_wO2 = case_when(str_detect(spo2_o2_1, "Yes")  ~ spo2_1))%>%
  mutate(spO2_1_woO2 = case_when(str_detect(spo2_o2_1, "No")  ~ spo2_1))%>%
  
  mutate(spO2_2_wO2 = case_when(str_detect(spo2_o2_2, "Yes")  ~ spo2_2))%>%
  mutate(spO2_2_woO2 = case_when(str_detect(spo2_o2_2, "No")  ~ spo2_2))%>%
  
  #using NEWS to make a reverse
  mutate(spO2_1_cat = case_when(str_detect(spo2_o2_1, "Yes") & spo2_1 > 97 ~ 1,
                                str_detect(spo2_o2_1, "Yes") & spo2_1 > 95 ~ 2,
                                str_detect(spo2_o2_1, "Yes") & spo2_1 > 93 ~ 3,
                                str_detect(spo2_o2_1, "Yes") & spo2_1 > 88 ~ 4,
                                str_detect(spo2_o2_1, "No") & spo2_1 > 93 ~ 4,
                                str_detect(spo2_o2_1, "No") & spo2_1 > 86 ~ 3,
                                str_detect(spo2_o2_1, "No") & spo2_1 > 83 ~ 2,
                                str_detect(spo2_o2_1, "No") & spo2_1 < 83 ~ 1))%>%
  
  
  mutate(spO2_2_cat = case_when(str_detect(spo2_o2_2, "Yes") & spo2_2 > 97 ~ 1,
                                str_detect(spo2_o2_2, "Yes") & spo2_2 > 95 ~ 2,
                                str_detect(spo2_o2_2, "Yes") & spo2_2 > 93 ~ 3,
                                str_detect(spo2_o2_2, "Yes") & spo2_2 > 88 ~ 4,
                                str_detect(spo2_o2_2, "No") & spo2_2 > 93 ~ 4,
                                str_detect(spo2_o2_2, "No") & spo2_2 > 86 ~ 3,
                                str_detect(spo2_o2_2, "No") & spo2_2 > 83 ~ 2,
                                str_detect(spo2_o2_2, "No") & spo2_2 < 83 ~ 1))


#måste nu tillfogar dessa som variabler til codebook
newnames<-data.frame("delay2ifdirect","rts_sbp1","rts_sbp2","rts_rr1","rts_rr2","ambulancefromthescene","tranclass","bl_rec","sc_hi","gcs_v_1_class","gcs_m_1_class","gcs_e_1_class","gcs_v_2_class","gcs_m_2_class","gcs_e_2_class","spO2_1_wO2","spO2_1_woO2","spO2_2_wO2","spO2_2_woO2","spO2_1_cat","spO2_2_cat")
newlabels<-data.frame("delay2ifdirect","revised trauma score sbp1","revised trauma score sbp2","revised trauma score rr1","revised trauma score rr2","ambulancefromthescene","tranclass","blood received 1st hour", "serum creatinine high","gcs_v_1_class","gcs_m_1_class","gcs_e_1_class","gcs_v_2_class","gcs_m_2_class","gcs_e_2_class","spO2_1_wO2","spO2_1_woO2","spO2_2_wO2","spO2_2_woO2","spO2_1_cat","spO2_2_cat")
newtypes<-data.frame("quantitative","quantitative","quantitative","quantitative","quantitative","qualitative","qualitative","qualitative","qualitative","qualitative","qualitative","qualitative","qualitative","qualitative","qualitative","quantitative","quantitative","quantitative","quantitative","qualitative","qualitative")
#men jag vet inte hur jag lägger till nya rader så...
headersnew = data.frame(matrix(,nrow = nrow(codebook)+21, ncol = ncol(codebook)-1))
headersnew[1:193,1:ncol(codebook)]<-data.frame(codebook)
colnames(headersnew)<-(colnames(codebook))
#start=220 #nrow(headerproperties)+1
#endat1=233 #nrow(headerproperties)+3
count=0
for (a in 1:21){
  count=count+1
  headersnew[nrow(codebook)+count,1]=newnames[1,count]
  headersnew[nrow(codebook)+count,2]=newlabels[1,count]
  headersnew[nrow(codebook)+count,6]=newtypes[1,count]
}
count=0
#fixar att type hamnat på note i dessa rader
for (a in 69:193){
  count=count+1
  misplacedtype=headersnew[a,5]
  headersnew[a,6]=misplacedtype
  
}

codebook=headersnew

# 3. Set seed and split into training and test samples.
# The seed allows us to always get the same randomized sample when we run the code.
#set.seed(46)
# Use 60% of the data for training
train_data <- data %>% sample_frac(.60)
# Use the remaining 40% as test data
test_data <- anti_join(data, train_data, by = 'seqn')
# Optional, Verify that no rows overlap, should return 0
count(train_data %>% filter(seqn %in% test_data$seqn))

# 4. Compare variables between ICU and non-ICU patients
# Define the categorical and continuous variables
cat_variables <- codebook %>% filter(type %in% "qualitative") %>%
  # We filter out pid, not to include that in our analysis
  filter(name != "pid") %>% pull(name)
cont_variables <- codebook %>% filter(type %in% "quantitative") %>%
  # We filter out licu since this is our outcome
  filter(!name %in% c("licu")) %>% pull(name)

# Look at the data using table one, jag fattar inte hur jag kan se om inte variabelnamn
tableone::CreateContTable(data = train_data, vars = cont_variables, strata = "treated_icu")
tableone::CreateCatTable(data = train_data, vars = cat_variables, strata = "treated_icu")

look_cont1=tableone::CreateContTable(data = test_data, vars = cont_variables, strata = "treated_icu")
look_cat1=tableone::CreateCatTable(data = test_data, vars = cat_variables, strata = "treated_icu")

#nu lägger vi på själva analysen. Vi har alla nya skapade parametrar exkl ambulansransporten med

predictors=data
#"rts_sbp1","rts_sbp2","rts_rr1","rts_rr2","tranclass","bl_rec","sc_hi","gcs_v_1_class","gcs_m_1_class","gcs_e_1_class","gcs_v_2_class","gcs_m_2_class","gcs_e_2_class", "spO2_1_wO2","spO2_1_woO2","spO2_2_wO2","spO2_2_woO2"

#samt de gamla
#..... kolla i saker jag vill ta upp
#died, NISS, Operation 1a timmen, Mode of injury, transfer, age sex

#predictors=data.frame(data["rts_sbp1”],data[”rts_sbp2”],data[”rts_rr1”],data[”rts_rr2”],data[”tranclass”],data[”bl_rec”],data[”sc_hi”],data[”gcs_v_1_class”],data[”gcs_m_1_class”],data[”gcs_e_1_class”],data[”gcs_v_2_class”],data[”gcs_m_2_class”],data[”gcs_e_2_class”], data["spO2_1_wO2"],data[”spO2_1_woO2”],data[”spO2_2_wO2”],data[”spO2_2_woO2"],data["NISS"],data["age"],data["sex"],data["age"],data["op_1"],data["moi"], data["tran"],data["died"])

predictors = data  %>% select("rts_sbp1","rts_sbp2","rts_rr1","rts_rr2","tranclass","bl_rec","sc_hi","gcs_v_1_class","gcs_m_1_class","gcs_e_1_class","gcs_v_2_class","gcs_m_2_class","gcs_e_2_class", "spO2_1_wO2","spO2_1_woO2","spO2_2_wO2","spO2_2_woO2", "niss", "age", "sex", "age", "ot_1", "moi", "tran", "died","spO2_1_cat","spO2_2_cat","died" )

#titta på kont värdena och bedöm
continuous <-select_if(predictors, is.numeric)
summary(continuous)

# Histogram with kernel density curve, exempel med ålder
ggplot(continuous, aes(x = age)) +
  geom_density(alpha = .2, fill = "#FF6666")


# Select categorical column
factor <- data.frame(select_if(predictors, is.character))
ncol(factor)

# Create graph for each column
graph <- lapply(names(factor),
                function(x)
                  ggplot(factor, aes(get(x))) +
                  geom_bar() +
                  theme(axis.text.x = element_text(angle = 90)))
graph

#addera outputten till prediktor data framen
treated_icu=data$treated_icu
tran=data$tran
predictors$treated_icu=treated_icu

#make 2 factor
predictors$bl_rec <- as.factor(predictors$bl_rec)
predictors$sc_hi <- as.factor(predictors$sc_hi)
predictors$sex <- as.factor(predictors$sex)
predictors$tran <- as.factor(predictors$tran)
predictors$tranclass <- as.factor(predictors$tranclass)
predictors$ot_1 <- as.factor(predictors$ot_1)
predictors$moi <- as.factor(predictors$moi)
predictors$tran <- as.factor(predictors$tran)
predictors$died <- as.factor(predictors$died)

#make integer
predictors$rts_sbp1 <- as.integer(predictors$rts_sbp1)
predictors$rts_sbp2 <- as.integer(predictors$rts_sbp2)
predictors$rts_rr1 <- as.integer(predictors$rts_rr1)
predictors$rts_rr2 <- as.integer(predictors$rts_rr2)
predictors$gcs_v_1_class <- as.integer(predictors$gcs_v_1_class)
predictors$gcs_m_1_class <- as.integer(predictors$gcs_m_1_class)
predictors$gcs_e_1_class <- as.integer(predictors$gcs_e_1_class)
predictors$gcs_v_2_class <- as.integer(predictors$gcs_v_2_class)
predictors$gcs_m_2_class <- as.integer(predictors$gcs_m_2_class)
predictors$gcs_e_2_class <- as.integer(predictors$gcs_e_2_class)


#funkar inte
#model=glm2(treated_icu~rts_sbp1+rts_sbp2+rts_rr1+rts_rr2+tranclass+bl_rec+sc_hi+gcs_v_1_class+gcs_m_1_class+gcs_e_1_class+gcs_v_2_class+gcs_m_2_class+gcs_e_2_class+spO2_1_wO2+spO2_1_woO2+spO2_2_wO2+spO2_2_woO2+ niss+ age+ sex+ ot_1+ moi+ tran+ died,data=predictors, family=binomial)

#funkar
#model=glm2(treated_icu~rts_sbp1+rts_sbp2+rts_rr1+rts_rr2+tranclass+bl_rec+sc_hi+gcs_v_1_class+gcs_e_1_class+gcs_v_2_class+gcs_m_2_class+gcs_e_2_class+niss+age+sex+died+ot_1+spO2_1_cat,data=predictors, family=binomial)

#funkar
glm.fit=glm2(treated_icu~rts_sbp1+rts_sbp2+rts_rr1+rts_rr2+tranclass+bl_rec+sc_hi+gcs_v_1_class+gcs_m_1_class+gcs_e_1_class+gcs_v_2_class+gcs_m_2_class+gcs_e_2_class+ niss+ age+ sex+ ot_1+ moi+ tran+ died+spO2_1_cat+spO2_2_cat,data=d, family=binomial)

#spO2_1_woO2+spO2_2_wO2+spO2_2_woO2 funkar inte, eftersom de har så många NA

map(predictors, ~sum(is.na(.)))
summary(predictors)
sapply(predictors, table)

#okej, nu ska jag bedöma hur bra det blev. Steg 1 verkar vara att återskapa prediktor-dataframen så som den var innan jag la till beroende variabeln
#predictors <- predictors %>% pull(treted_icu)

# predictors = data  %>% select("rts_sbp1","rts_sbp2","rts_rr1","rts_rr2","tranclass","bl_rec","sc_hi","gcs_v_1_class","gcs_m_1_class","gcs_e_1_class","gcs_v_2_class","gcs_m_2_class","gcs_e_2_class", "spO2_1_wO2","spO2_1_woO2","spO2_2_wO2","spO2_2_woO2", "niss", "age", "sex", "age", "ot_1", "moi", "tran", "died","spO2_1_cat","spO2_2_cat" )
#
# #make 2 factor
# predictors$bl_rec <- as.factor(predictors$bl_rec)
# predictors$sc_hi <- as.factor(predictors$sc_hi)
# predictors$sex <- as.factor(predictors$sex)
# predictors$tran <- as.factor(predictors$tran)
# predictors$tranclass <- as.factor(predictors$tranclass)
# predictors$ot_1 <- as.factor(predictors$ot_1)
# predictors$moi <- as.factor(predictors$moi)
# predictors$tran <- as.factor(predictors$tran)
# predictors$died <- as.factor(predictors$died)
#
# #make integer
# predictors$rts_sbp1 <- as.integer(predictors$rts_sbp1)
# predictors$rts_sbp2 <- as.integer(predictors$rts_sbp2)
# predictors$rts_rr1 <- as.integer(predictors$rts_rr1)
# predictors$rts_rr2 <- as.integer(predictors$rts_rr2)
# predictors$gcs_v_1_class <- as.integer(predictors$gcs_v_1_class)
# predictors$gcs_m_1_class <- as.integer(predictors$gcs_m_1_class)
# predictors$gcs_e_1_class <- as.integer(predictors$gcs_e_1_class)
# predictors$gcs_v_2_class <- as.integer(predictors$gcs_v_2_class)
# predictors$gcs_m_2_class <- as.integer(predictors$gcs_m_2_class)
# predictors$gcs_e_2_class <- as.integer(predictors$gcs_e_2_class)

treated_icu=data.frame(treated_icu)

fitval=glm.fit$fitted.values

#lines(treated_icu, glm.fit$fitted.values)
#roc(predictors, glm.fit$fitted.values, plot=TRUE)



################# Johanna ##################
# To check the model
summary(glm.fit)
# Here we see that, using the test data in the way it was treated above, 14115 observations were deleted and not included in the model due to missing values.
# This is because each entry containing an NA for a predictor variable that is not a factor, will be removed. That's why there's an error with unknown factor levels when you try to apply the model on the data
# The model is acutally built on just 16000 - 14115 observations. To improve this, we remove cases where we have continious variables containg NA (Better to be explicit and not let the model drop them)
# We convert NA to a category, Missing, that way it gets treated as another category and not NA (and hence get dropped).

####### Data pre-processing for modeling #######
# We can't use any observations that has NA in the outcome we predict to, we delete these.
train_data <- train_data %>% filter(!is.na(treated_icu))
# Select the predictors and outcome
model_data <- train_data %>% select("treated_icu", "rts_sbp1","rts_sbp2","rts_rr1","rts_rr2","tranclass","bl_rec","sc_hi","gcs_v_1_class","gcs_m_1_class","gcs_e_1_class","gcs_v_2_class","gcs_m_2_class","gcs_e_2_class", "spO2_1_wO2","spO2_1_woO2","spO2_2_wO2","spO2_2_woO2", "niss", "age", "sex", "age", "ot_1", "moi", "tran", "died","spO2_1_cat","spO2_2_cat" )
# Analyse missing
library(naniar)
# Visulise number of missing for each variable
vis_miss(model_data)
# We will treat all variables, except for age, and niss as categorical. This based on them being categorical variables or having a large amout of missing that we will treat as categorical.
# For age and niss, we remove observations that contain NA for these variables.
model_data <- model_data %>% filter(!is.na(niss), !is.na(age))
# For the rest, all to be treated as categorical variables, replace NA with "Missing", for it to be modeled as it's own category. This will automaticly convert for exampel numeric cariables to characters.
model_data <- model_data %>% replace(is.na(.), "Missing")

# Handle the test data in the same way as the training data
test_data <- test_data %>% select("treated_icu", "rts_sbp1","rts_sbp2","rts_rr1","rts_rr2","tranclass","bl_rec","sc_hi","gcs_v_1_class","gcs_m_1_class","gcs_e_1_class","gcs_v_2_class","gcs_m_2_class","gcs_e_2_class", "spO2_1_wO2","spO2_1_woO2","spO2_2_wO2","spO2_2_woO2", "niss", "age", "sex", "age", "ot_1", "moi", "tran", "died","spO2_1_cat","spO2_2_cat" )
test_data <- test_data %>% filter(!is.na(treated_icu))
test_data <- test_data %>% filter(!is.na(niss), !is.na(age))
test_data <- test_data %>% replace(is.na(.), "Missing")

# Fot the model using the training data
glm_model <- glm(treated_icu ~ rts_sbp1+rts_sbp2+rts_rr1+rts_rr2+tranclass+bl_rec+sc_hi+gcs_v_1_class+gcs_m_1_class+gcs_e_1_class+gcs_v_2_class+gcs_m_2_class+gcs_e_2_class+niss+age+sex+ot_1+moi+tran+died+spO2_1_cat+spO2_2_cat,
                 family=binomial, data = model_data)
summary(glm_model)

# Use the model to create ROC curve and calculate AUC on the training data
train_prob <- predict(glm_model, newdata = model_data, type = "response")
train_roc = roc(model_data$treated_icu ~ train_prob, plot = TRUE, print.auc = TRUE)
# Apply the model to the test data and calculate AUC
test_prob <- predict(glm_model, newdata = test_data, type = "response")
test_roc = roc(test_data$treated_icu ~ test_prob, plot = TRUE, print.auc = TRUE)

### Matching/Youdens statistic ###


#youdens bedömer ROC-objektet. den anger vilken cutoff som ger bäst specificitet och sensitivity
youdens_train=coords(train_roc, x="best", input="threshold", best.method="youden")
youdens_test=coords(test_roc, x="best", input="threshold", best.method="youden")

#cutoff
cutoff_train=youdens_train$threshold
cutoff_test=youdens_train$threshold

#unmet
#sen räknar vi hur många pat fått högre propensity scores av modellen än den cutoffen . det är så många som har tillräckligt höga propensity scores att de rimligen borde ha ICU-vårdats. 
overcutoff_train=sum(train_prob>cutoff_train)
overcutoff_test=sum(test_prob>cutoff_test)
# och genom att sätta denna beräkning i relation till hur många som faktiskt ICU-vårdades, som vi kan uttala oss om underskottet på ICU sängar. 
ICUtrain=sum(train_data$treated_icu,na.rm=T)
ICUtest=sum(test_data$treated_icu,na.rm=T)
unmet_train=overcutoff_train-ICUtrain
unmetprop_train=unmet_train/ICUtrain
unmet_test=overcutoff_test-ICUtest
unmetprop_test=unmet_test/ICUtest

# return(unmetprop_test) #return unmet 
# }

#perform bootstrapping with 2000 replications
#reps <- boot(data=predictors, statistic=unmet_function, R=15, formula=mpg~disp)

#view results of boostrapping
#reps


