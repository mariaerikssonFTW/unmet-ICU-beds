library(tidyverse)
library(dplyr)
library(tableone)
library(lubridate)
library(noacsr)
library(glm2)
library(ggplot2)
library(pROC)
library(boot)

#setwd("C:/Users/maria/Downloads/Packt Learning RStudio for R Statistical Computing 2012 RETAIL eBook-repackb00k/unmet-ICU-beds")
source("johanna/functions.R")

# 1. Import the data
#
# Read the dataset and codebook from github
data <- import_csv_from_github("https://github.com/titco/titco-I/blob/master/titco-I-full-dataset-v1.csv")
#data <- import_csv_from_github("https://github.com/titco/titco-I/blob/master/titco-I-limited-dataset-v1.csv")
codebook <- import_csv_from_github("https://github.com/titco/titco-I/blob/master/codebook-titco-I-v1.csv")


# #för jag vill kunna köra offline
# save(data, file = "data.RData")
# save(codebook, file = "codebook.RData")
# 
# load("data.RData")
# load("codebook.RData")

unmet_function <- function(formula, data, indices) {

#first add all timer data and make time calculations. 
  #This section was used for investigations that ended up fruitless and it is currently not in any practical use, go ahead and skip ahead to line 
  #data=data.frame( data, outcome)
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

  mutate(stillalive_6h = case_when(time_to_death > 6 ~ 1,
                                   time_to_death < 6 ~ 0)) %>%

  mutate(delay2 = round(as.numeric(arrival_time-injury_time , units = "hours"),0))%>%
  mutate(delay23 = round(as.numeric(firstsurvey_time-injury_time , units = "hours"),0))%>%
  mutate(delay2ifdirect = case_when(str_detect(tran, "No") & delay2<72 ~ delay2)) # %>%

#addera kolumnerna
data=data.frame( data, outcome)

#data cleaning, create two columns for bad GCS scores to be discarded
#filter for GCS that don't quite add up
data <- data %>%
  
  mutate(gcs_e_1int = case_when(str_detect(gcs_e_1, "1c")   ~ 1,
                                str_detect(gcs_e_1, "1")  ~ 1,
                                str_detect(gcs_e_1, "2")   ~ 2,
                                str_detect(gcs_e_1, "3")   ~ 3,
                                str_detect(gcs_e_1, "4")   ~ 4,
                                str_detect(gcs_e_1, "5")   ~ 5,
                                str_detect(gcs_e_1, "6")   ~ 6)) %>%
  
  mutate(gcs_v_1int = case_when(str_detect(gcs_v_1, "1")  ~ 1,
                                str_detect(gcs_v_1, "1v")  ~ 1,
                                str_detect(gcs_v_1, "2")   ~ 2,
                                str_detect(gcs_v_1, "3")   ~ 3,
                                str_detect(gcs_v_1, "4")   ~ 4,
                                str_detect(gcs_v_1, "5")   ~ 5,
                                str_detect(gcs_v_1, "6")   ~ 6))%>%
  
  mutate(gcs_e_2int = case_when(str_detect(gcs_e_2, "1c")   ~ 1,
                                str_detect(gcs_e_2, "1")  ~ 1,
                                str_detect(gcs_e_2, "2")   ~ 2,
                                str_detect(gcs_e_2, "3")   ~ 3,
                                str_detect(gcs_e_2, "4")   ~ 4,
                                str_detect(gcs_e_2, "5")   ~ 5,
                                str_detect(gcs_e_2, "6")   ~ 6)) %>%
  
  mutate(gcs_v_2int = case_when(str_detect(gcs_v_2, "1")  ~ 1,
                                str_detect(gcs_v_2, "1v")  ~ 1,
                                str_detect(gcs_v_2, "2")   ~ 2,
                                str_detect(gcs_v_2, "3")   ~ 3,
                                str_detect(gcs_v_2, "4")   ~ 4,
                                str_detect(gcs_v_2, "5")   ~ 5,
                                str_detect(gcs_v_2, "6")   ~ 6))

data$gcs_e_1int <- as.integer(data$gcs_e_1int)
data$gcs_v_1int <- as.integer(data$gcs_v_1int)
data$gcs_m_1 <- as.integer(data$gcs_m_1)
sum_individual_1=data$gcs_e_1int +data$gcs_v_1int+data$gcs_m_1
diff_1=sum_individual_1-data$gcs_t_1

data$gcs_e_2int <- as.integer(data$gcs_e_2int)
data$gcs_v_2int <- as.integer(data$gcs_v_2int)
data$gcs_m_2 <- as.integer(data$gcs_m_2)
sum_individual_2=data$gcs_e_2int +data$gcs_v_2int+data$gcs_m_2
diff_2=sum_individual_2-data$gcs_t_2


data=data.frame( data, diff_1,diff_2)  

data <- data %>% 
  mutate(weird_gcs_1=case_when(diff_1 ==0  ~ 0))%>%
  mutate(weird_gcs_2=case_when(diff_2 ==0  ~ 0))

#more cleaning
# Updates to the dataset
data <- data %>%
  # Convert age to numeric
  mutate(age = parse_number(age)) %>%
  #applicera överenskomna filtreringar
  #återkom om beslut placeholcer:  filter(data$stillalive==1)%>%
  
  #kategorisera blodtryck enligt RTS. bonus tvätt: alla med sbp =0 men puls blir NA
  mutate(rts_sbp1 = case_when(sbp_1 ==0 & hr_1 <1 & sbp_1 !='NA'  ~ 0,
                              sbp_1 < 49 & sbp_1 !='NA' ~ 1,
                              sbp_1 < 75 & sbp_1 !='NA' ~ 2,
                              sbp_1 < 89 & sbp_1 !='NA' ~ 3,
                              sbp_1 < 300 & sbp_1 !='NA' ~4)) %>%
  
  mutate(rts_sbp2 = case_when(sbp_2 ==0 & hr_2<1 & sbp_2 !='NA' ~ 0,
                              sbp_2 < 49  & sbp_2 !='NA' ~ 1,
                              sbp_2 < 75  & sbp_2 !='NA' ~ 2,
                              sbp_2 < 89   & sbp_2 !='NA' ~ 3,
                              sbp_2 < 300 & sbp_2 !='NA' ~ 4)) %>%
  
  #kategorisera puls enligt NEWS (typ) bonus tvätt: alla med hr =0 men blodtryck blir NA
  mutate(cat_hr1 = case_when(hr_1 ==0 & sbp_1 <1 & hr_1 !='NA'  ~ 0,
                             hr_1< 41 & hr_1 !='NA' ~ 1,
                             hr_1 < 51 & hr_1 !='NA' ~ 3,
                             hr_1 < 89 & hr_1 !='NA' ~ 4,
                             hr_1 < 111 & hr_1 !='NA' ~3,
                             hr_1 < 131 & hr_1 !='NA' ~2,
                             hr_1 > 131 & hr_1 !='NA' ~1,)) %>%
  
  mutate(cat_hr2 = case_when(hr_2 ==0 & sbp_1 <1 & hr_2 !='NA'  ~ 0,
                             hr_2< 41 & hr_2 !='NA' ~ 1,
                             hr_2 < 51 & hr_2 !='NA' ~ 3,
                             hr_2 < 89 & hr_2 !='NA' ~ 4,
                             hr_2 < 111 & hr_2 !='NA' ~3,
                             hr_2 < 131 & hr_2 !='NA' ~2,
                             hr_2 > 131 & hr_2 !='NA' ~1,)) %>%
  
  
  #kategorisera resp rate enligt RTS. 
  mutate(rts_rr1 = case_when(rr_1 ==0   ~ 0,
                             rr_1 < 5   ~ 1,
                             rr_1 < 9   ~ 2,
                             rr_1 > 29   ~ 3,
                             rr_1 > 9 & rr_1 < 29   ~ 4)) %>%
  
  mutate(rts_rr2 = case_when(rr_2 ==0   ~ 0,
                             rr_2 < 5   ~ 0, # 1, måste klumpas ihop för att inte bli för få i en kategori
                             rr_2 < 9   ~0 , # 2, kan klumpas ihop för att inte bli för få i en kategori
                             rr_2 > 29   ~ 3,
                             rr_2 > 9 & rr_2 < 29 ~ 4)) %>%
  
  #kommit direkt från skadeplatsen?
  mutate(ambulancefromthescene = case_when(str_detect(tran, "No") & str_detect(mot, "Ambulance") ~ 1)) %>%
  
#...för att bygga våra tre transport kategorier
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
                          sc<3.000001  ~ "No")) %>% 
  
  
  mutate(gcs_v_1_class= case_when(str_detect(gcs_v_1, "1") &weird_gcs_1==0   ~ 1,
                                  str_detect(gcs_v_1, "1v")   ~ 1, #vill ha egen kategori 2
                                  str_detect(gcs_v_1, "2")  &weird_gcs_1==0  ~ 3,
                                  str_detect(gcs_v_1, "3" ) &weird_gcs_1==0  ~ 4,
                                  str_detect(gcs_v_1, "4")  &weird_gcs_1==0  ~ 4,
                                  str_detect(gcs_v_1, "5" ) &weird_gcs_1==0  ~ 4,
                                  str_detect(gcs_v_1, "6" ) &weird_gcs_1==0  ~ 4)) %>%
  
  mutate(gcs_m_1_class= case_when(str_detect(gcs_m_1, "1") &weird_gcs_1==0    ~ 1,
                                  str_detect(gcs_m_1, "2")  &weird_gcs_1==0   ~ 2,
                                  str_detect(gcs_m_1, "3" )  &weird_gcs_1==0  ~ 3,
                                  str_detect(gcs_m_1, "4")  &weird_gcs_1==0   ~ 3,
                                  str_detect(gcs_m_1, "5" ) &weird_gcs_1==0   ~ 3,
                                  str_detect(gcs_m_1, "6" ) &weird_gcs_1==0   ~ 3)) %>%
  
  mutate(gcs_e_1_class= case_when(str_detect(gcs_e_1, "1")  &weird_gcs_1==0   ~ 1,
                                  str_detect(gcs_e_1, "1c")   ~ 1, #vill ha egen kategori 2
                                  str_detect(gcs_e_1, "2")  &weird_gcs_1==0   ~ 3,
                                  str_detect(gcs_e_1, "3" ) &weird_gcs_1==0  ~ 4,
                                  str_detect(gcs_e_1, "4") &weird_gcs_1==0    ~ 4,
                                  str_detect(gcs_e_1, "5" ) &weird_gcs_1==0   ~ 4,
                                  str_detect(gcs_e_1, "6" ) &weird_gcs_1==0   ~ 4)) %>%
  
  mutate(gcs_v_2_class= case_when(str_detect(gcs_v_2, "1")  &weird_gcs_2==0  ~ 1,
                                  str_detect(gcs_v_2, "1v")  ~ 1, #vill ha egen kategori 2
                                  str_detect(gcs_v_2, "2") &weird_gcs_2==0   ~ 3,
                                  str_detect(gcs_v_2, "3" )  &weird_gcs_2==0 ~ 4,
                                  str_detect(gcs_v_2, "4")  &weird_gcs_2==0  ~ 4,
                                  str_detect(gcs_v_2, "5" )  &weird_gcs_2==0 ~ 4,
                                  str_detect(gcs_v_2, "6" ) &weird_gcs_2==0 ~ 4)) %>%
  
  mutate(gcs_m_2_class= case_when(str_detect(gcs_m_2, "1")  &weird_gcs_2==0  ~ 1,
                                  str_detect(gcs_m_2, "2")  &weird_gcs_2==0  ~ 2,
                                  str_detect(gcs_m_2, "3" ) &weird_gcs_2==0  ~ 3,
                                  str_detect(gcs_m_2, "4")  &weird_gcs_2==0  ~ 3,
                                  str_detect(gcs_m_2, "5" ) &weird_gcs_2==0  ~ 3,
                                  str_detect(gcs_m_2, "6" )  &weird_gcs_2==0 ~ 3)) %>%
  
  mutate(gcs_e_2_class= case_when(str_detect(gcs_e_2, "1") &weird_gcs_2==0   ~ 1,
                                  str_detect(gcs_e_2, "1c")  ~ 1, #vill ha egen kategori 2
                                  str_detect(gcs_e_2, "2")  &weird_gcs_2==0  ~ 3,
                                  str_detect(gcs_e_2, "3" )  &weird_gcs_2==0 ~ 4,
                                  str_detect(gcs_e_2, "4")  &weird_gcs_2==0  ~ 4,
                                  str_detect(gcs_e_2, "5" ) &weird_gcs_2==0  ~ 4,
                                  str_detect(gcs_e_2, "6" ) &weird_gcs_2==0  ~ 4)) %>%
  
  # Add our outcome, if patient has a recorded length in the ICU then treated_icu = 1
  #om SION,>24h för att räknas som ICU-pat
  mutate(treated_icu = case_when(licu > 24 & hos ==6273 ~ 1,
                                 licu < 25 & hos ==6273 ~ 0,
                                 licu > 0 & hos ==7215 ~ 1,
                                 licu > 0 & hos ==7842 ~ 1,
                                 licu > 0 & hos ==8264 ~ 1,
                                 licu == 0 & hos ==7215 ~ 0,
                                 licu == 0 & hos ==7842 ~ 0,
                                 licu == 0 & hos ==8264 ~ 0)) %>% 
  
  
  mutate(spO2_1_wO2 = case_when(str_detect(spo2_o2_1, "Yes")  ~ spo2_1))%>%
  mutate(spO2_1_woO2 = case_when(str_detect(spo2_o2_1, "No")  ~ spo2_1))%>%
  
  mutate(spO2_2_wO2 = case_when(str_detect(spo2_o2_2, "Yes")  ~ spo2_2))%>%
  mutate(spO2_2_woO2 = case_when(str_detect(spo2_o2_2, "No")  ~ spo2_2))%>%
  
  #using NEWS. i NEWS är låga scores bra till skillnad från RTS, men för kategorisering för GLM2 spelar det ingen roll.
  mutate(spO2_1_cat = case_when(str_detect(spo2_o2_1, "Yes") & spo2_1 > 97 ~ 3,
                                str_detect(spo2_o2_1, "Yes") & spo2_1 > 95 ~ 2,
                                str_detect(spo2_o2_1, "Yes") & spo2_1 > 93 ~ 1,
                                str_detect(spo2_o2_1, "Yes") & spo2_1 > 88 ~ 0,
                                str_detect(spo2_o2_1, "No") & spo2_1 > 93 ~ 0,
                                str_detect(spo2_o2_1, "No") & spo2_1 > 86 ~ 1,
                                str_detect(spo2_o2_1, "No") & spo2_1 > 83 ~ 2,
                                str_detect(spo2_o2_1, "No") & spo2_1 < 83 ~ 3))%>%
  
  
  mutate(spO2_2_cat = case_when(str_detect(spo2_o2_2, "Yes") & spo2_2 > 97 ~ 3,
                                str_detect(spo2_o2_2, "Yes") & spo2_2 > 95 ~ 2,
                                str_detect(spo2_o2_2, "Yes") & spo2_2 > 93 ~ 1,
                                str_detect(spo2_o2_2, "Yes") & spo2_2 > 88 ~ 0,
                                str_detect(spo2_o2_2, "No") & spo2_2 > 93 ~ 0,
                                str_detect(spo2_o2_2, "No") & spo2_2 > 86 ~ 1,
                                str_detect(spo2_o2_2, "No") & spo2_2 > 83 ~ 2,
                                str_detect(spo2_o2_2, "No") & spo2_2 < 83 ~ 3))%>%

  #se om modellen klarar av lov length of ventilation som prediktor (97% ICU admission, ≠ 100 % pga KEM ventilerade i resuscitation room). det verkade inte gå.
  mutate(lov_cat = case_when(lov > 0  ~ 1,
                             lov == 0  ~ 0))%>%

  # ny parameter incl, bedömning av project officer vid ankomst. 3="Admitted with potentially life-threatening injury as assessed by treating physician"
  #bonus: kat 2 "History of any of the below specified injury mechanisms but died between arrival and admission" filtreras bort

mutate(doctor_assess = case_when(incl ==3  ~ 1,
                           incl == 1  ~ 0,
                           incl == 0  ~ 0))


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

#här borde assigneringen för funktionen unmet_function, vara. efter all mangling av datan
d <- data[indices,] #allows boot to select sample

# 3. Set seed and split into training and test samples.
# The seed allows us to always get the same randomized sample when we run the code.
#set.seed(46)
# Use 60% of the data for training
train_data <- data %>% sample_frac(.60)
# Use the remaining 40% as test data
test_data <- anti_join(data, train_data, by = 'seqn')
# Optional, Verify that no rows overlap, should return 0
count(train_data %>% filter(seqn %in% test_data$seqn))

# Define the categorical and continuous variables, pull outcome and admin parameters
cat_variables <- codebook %>% filter(type %in% "qualitative") %>%
  # We filter out patient ID, not to include that in our analysis
  filter(name != "pid") %>% pull(name)
cont_variables <- codebook %>% filter(type %in% "quantitative") %>%
  # We filter out licu since this is our outcome
  filter(!name %in% c("licu")) %>% pull(name)

#kod för titta-och-utvärdera
tableone::CreateContTable(data = train_data, vars = cont_variables, strata = "treated_icu")
tableone::CreateCatTable(data = train_data, vars = cat_variables, strata = "treated_icu")
look_cont1=tableone::CreateContTable(data = test_data, vars = cont_variables, strata = "treated_icu")
look_cat1=tableone::CreateCatTable(data = test_data, vars = cat_variables, strata = "treated_icu")

#följande sektion gör en modell på hela datat. den är kvar som kontroll, en modell på hela datan borde prestera likvärdigt som modellen byggd på testdatat

predictors = data  %>% select("rts_sbp1","rts_sbp2","rts_rr1","rts_rr2","tranclass","bl_rec","sc_hi","gcs_v_1_class","gcs_m_1_class","gcs_e_1_class","gcs_v_2_class","gcs_m_2_class","gcs_e_2_class", "spO2_1_wO2","spO2_1_woO2","spO2_2_wO2","spO2_2_woO2", "niss", "age", "sex", "age", "ot_1", "moi", "tran", "died","spO2_1_cat","spO2_2_cat","died","cat_hr1", "cat_hr2","gcs_t_1","gcs_t_2","lov_cat","saw_1","intub_1","icd_1","doctor_assess")
#notera tillägg av "saw_1","intub_1","icd_1","doctor_assess". enligt diskussion med Johanna.dessa är inte 100% ICU pga olika omständigheter. Lov length of ventilation är inte heller 1:1 men påverkade modellen för mycket

#addera outputten till prediktor data framen
treated_icu=data$treated_icu
tran=data$tran
predictors$treated_icu=treated_icu

# predictors <- predictors %>% filter(!is.na(treated_icu))
# predictors <- predictors %>% filter(!is.na(niss), !is.na(age))
# predictors <- predictors %>% replace(is.na(.), "Missing")

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

glm.fit=glm2(treated_icu~rts_sbp1+rts_sbp2+rts_rr1+rts_rr2+tranclass+bl_rec+sc_hi+gcs_v_1_class+gcs_m_1_class+gcs_e_1_class+gcs_v_2_class+gcs_m_2_class+gcs_e_2_class+ niss+ age+ sex+ ot_1+ moi+ tran+ died+spO2_1_cat+spO2_2_cat+cat_hr1+cat_hr2+saw_1+intub_1+icd_1+doctor_assess,data=predictors, family=binomial)

#spO2_1_woO2+spO2_2_wO2+spO2_2_woO2 funkar inte, eftersom de har så många NA

map(predictors, ~sum(is.na(.)))
summary(predictors)
sapply(predictors,table)

################# Johanna ##################
# To check the model
summary(glm.fit)

####### Data pre-processing for modeling #######
# We can't use any observations that has NA in the outcome we predict to, we delete these.
train_data <- train_data %>% filter(!is.na(treated_icu))
# Select the predictors and outcome
model_data <- train_data %>% select("treated_icu", "rts_sbp1","rts_sbp2","rts_rr1","rts_rr2","tranclass","bl_rec","sc_hi","gcs_v_1_class","gcs_m_1_class","gcs_e_1_class","gcs_v_2_class","gcs_m_2_class","gcs_e_2_class", "spO2_1_wO2","spO2_1_woO2","spO2_2_wO2","spO2_2_woO2", "niss", "age", "sex", "age", "ot_1", "moi", "tran", "died","spO2_1_cat","spO2_2_cat","cat_hr1", "cat_hr2","lov_cat","saw_1","intub_1","icd_1","doctor_assess")

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
test_data <- test_data %>% select("treated_icu", "rts_sbp1","rts_sbp2","rts_rr1","rts_rr2","tranclass","bl_rec","sc_hi","gcs_v_1_class","gcs_m_1_class","gcs_e_1_class","gcs_v_2_class","gcs_m_2_class","gcs_e_2_class", "spO2_1_wO2","spO2_1_woO2","spO2_2_wO2","spO2_2_woO2", "niss", "age", "sex", "age", "ot_1", "moi", "tran", "died","spO2_1_cat","spO2_2_cat","cat_hr1", "cat_hr2","lov_cat","saw_1","intub_1","icd_1","doctor_assess")
test_data <- test_data %>% filter(!is.na(treated_icu))
test_data <- test_data %>% filter(!is.na(niss), !is.na(age))
test_data <- test_data %>% replace(is.na(.), "Missing")


# Fot the model using the training data
glm_model <- glm2(treated_icu ~ rts_sbp1+rts_sbp2+rts_rr1+rts_rr2+tranclass+bl_rec+sc_hi+gcs_v_1_class+gcs_m_1_class+gcs_e_1_class+gcs_v_2_class+gcs_m_2_class+gcs_e_2_class+niss+age+sex+ot_1+moi+tran+died+spO2_1_cat+spO2_2_cat+cat_hr1+cat_hr2+saw_1+intub_1+icd_1+doctor_assess,family=binomial, data = model_data)
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
cutoff_test=youdens_test$threshold

#unmet
#sen räknar vi hur många pat fått högre propensity scores av modellen än den cutoffen . det är så många som har tillräckligt höga propensity scores att de rimligen borde ha ICU-vårdats.
#röknar båda för kontroll
overcutoff_train=sum(train_prob>cutoff_train)
overcutoff_test=sum(test_prob>cutoff_test)
# och genom att sätta denna beräkning i relation till hur många som faktiskt ICU-vårdades, som vi kan uttala oss om underskottet på ICU sängar.
ICUtrain=sum(train_data$treated_icu,na.rm=T)
ICUtest=sum(test_data$treated_icu,na.rm=T)
unmet_train=overcutoff_train-ICUtrain
unmetprop_train=unmet_train/ICUtrain
unmet_test=overcutoff_test-ICUtest
unmetprop_test=unmet_test/ICUtest

#pga nyfiken, hur många vårdades på ICU trots att de hade för låga propensity scores för att "platsa"
df_prob=data.frame(test_prob)
df_question=data.frame(test_data,df_prob)
question <- df_question %>%
  
  mutate(unneccesary = case_when(df_prob <cutoff_test & treated_icu >0.5  ~ 1))

unneccesary_ICU=sum(question$unneccesary,na.rm=t)
unnecessary_prop=unneccesary_ICU/ICUtest

return(unmetprop_test) #return unmet
}

# #bootstrapping
# 
reps <- boot(data=data, statistic=unmet_function, R=4, formula=mpg~disp)

reps

#frågor:
#vad göra med serumkreatinin hi/lo? 12000 pat har data, 3000 är 0 (strikt taget orimligt lågt är det att tolka som "low" eller "odokuenterat"=NA?). det känns lite onödigt att kategoriserade dessa 3000 som missing. också är sc både hög och låg statistiskt signifikanta  í modellen
#icd_1 har 3 NA och när man höjer R i reps slumpar den till slut så att den får en i test men ingen i train eller vice versa (och error) . finns nån lösning på det?
#i summary(glm_model)
#incl = 2 alltså dött mellan arrival och admission. av incl har jag gjort en kategorisk variabel doctor_assess med kategorierna "3", "1 eller 0" och "2"=>NA=>missing. kategori 2 har dött mellan arrival och  admission och bör exkluderas. av dessa 120 pat har ändå 4 legat på ICU hur ska det tolkas? och hanteras? för de har jag gjort till missing och missing är en signifikant kategori 
#icd_1 har kategorierna "before arrival" "yes" och "no" varför finns bara "yes" och "no" i  summary(glm_model)
#intub_1 samma fråga som ovan och: i summary(glm_model) får man intub_1 = No som signifikant men det verkar orimligt och får inte stöd i tableone analysen
#look_cat1=tableone::CreateCatTable(data = test_data, vars = cat_variables, strata = "treated_icu")
#samma sak för gcs de låga GCS kategorierna motsvarande 1, 1v eller 1c och 2 är inte med i summary(glm_model) och det är höga gcs som är associerade med ICU? tableone enligt förväntan.
#för syst blodtryck och rr har höga RTS scores varit associerade med ICU enl summary(glm_model) det känns underligt men även tableone underlig. kod fel, förväntning fel, eller båda?
#även utan villkoret att gcs ska addera ihop rätt har jag enligt sapply(predictors, table) mycket få GCS_v_1_class och GCS_e_1_class (och _2) kategori 2 alltså 1v och 1c och det stämmer inte överens med sapply på data.frame(data$gcs_e_1, data$gcs_v_1,data$gcs_e_2, data$gcs_v_2). varför blir det så? jag är inte girig ffa jag vill ha 1v och 1c som egna kategorier men det förstör mina boots när de är så få på samma sätt som missing på icd_1 beskrivet ovan, därför är 1v och 1c ihopklumpade nu
#du ser jag räknat ut andel pat som varit på ICU utan att "platsa", för jämförelse mot unmet need. Johanna säger det är väntat såklart och inte finns standard för vad som är ok. finns det någon anständighetsgräns på denna proportion eller är det beyond the scope?