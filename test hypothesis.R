

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

  
#  mutate(weird_sc = case_when(sc ==0   ~ 1))

  
 # mutate(weird_sbp1 = case_when(sbp_1 ==0 & hr_1 >1 & sbp_1 !='NA'  ~ 1))%>%
 #    mutate(weird_sbp2 = case_when(sbp_2 ==0 & hr_2 >1 & sbp_2 !='NA'  ~ 1))%>%
 #    mutate(weird_hr1 = case_when(hr_1 ==0 & sbp_1 >1 & hr_1 !='NA'  ~ 1))%>%
 #    mutate(weird_hr2 = case_when(hr_2 ==0 & sbp_2 >1 & hr_2 !='NA'  ~ 1))
 # 
 # sum(question$weird_sbp1,na.rm=t)
 # sum(question$weird_sbp2,na.rm=t)
 # sum(question$weird_hr1,na.rm=t)
 # sum(question$weird_hr2,na.rm=t)
  
  # mutate(deadcategory = case_when(stillalive_6h ==0 & licu <1  ~ 0,
  # stillalive_6h ==0 & licu >1 & hos ==8264 ~ 1,
  # stillalive_6h ==0 & licu >1 & hos ==7215 ~ 1,
  # stillalive_6h ==0 & licu >1 & hos ==7842 ~ 1))%>%
  
#   
#   mutate(deadcategory = case_when(stillalive_6h ==0 & licu <1  ~ 0,
#                                   stillalive_6h ==0 & licu >1 & hos ==8264 ~ 1))%>%
#   mutate(diedhere = case_when(stillalive_6h ==0 & hos ==8264 ~ 1))%>%
#   
#   mutate(badtiming = case_when(time_to_death  <1   ~ 1))%>%
#   
#   mutate(timetoICU = case_when(str_detect(died, "Yes")  & licu >1 & time_to_death >1 ~ time_to_death))%>%
#   mutate(timetoICUlicu = case_when(str_detect(died, "Yes")  & licu >1 & time_to_death >1 ~ licu))
# 
# dead=16000-sum(question$stillalive_6h,na.rm=t)
# sum(question$deadcategory,na.rm=t)
# sum(question$diedhere,na.rm=t)
# 
# sum(question$badtiming,na.rm=t)
# sum(question$badtiming,na.rm=t)

# sum(question$weird_sc,na.rm=t)

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

sum(data$weird_gcs_1,na.rm=t)
sum(data$weird_gcs_2,na.rm=t)