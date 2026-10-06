library(tidyverse)
library(hms)
library(patchwork)

# Calculate background functions ------------------------------------------
Background_CO2 = function(data) {
  t = 30
  
  co2_background = rep(NA, nrow(data)) 
  co2_criteria   = rep(NA, nrow(data))
  data$date = as.POSIXct(data$date)
  
  for (i in 1:nrow(data)) {
    current_date = data$date[i]
    
    time_window_start = current_date - as.difftime(t, units = "secs")
    time_window_end = current_date + as.difftime(t, units = "secs")
    
    window_rows = which(data$date >= time_window_start & data$date <= time_window_end)
    
    if (length(window_rows) > 0) {
      co2_background[i] = mean(data$co2_ppm[window_rows], na.rm = TRUE)
      co2_criteria[i] = 2*sd(data$co2_ppm[window_rows], na.rm = TRUE)
    }
  }
  
  data$co2_background = co2_background
  data$co2_criteria   = co2_criteria
  
  return(data)  
}

Background_ch4 = function(data) {
  t = 30
  
  ch4_background = rep(NA, nrow(data))
  ch4_criteria = rep(NA, nrow(data))
  
  data$date = as.POSIXct(data$date)
  
  for (i in 1:nrow(data)) {
    current_date = data$date[i]
    
    time_window_start = current_date - as.difftime(t, units = "secs")
    time_window_end = current_date + as.difftime(t, units = "secs")
    
    window_rows = which(data$date >= time_window_start & data$date <= time_window_end)
    
    if (length(window_rows) > 0) {
      ch4_background[i] = mean(data$ch4_ppb[window_rows], na.rm = TRUE)
      ch4_criteria[i] = 2*sd(data$ch4_ppb[window_rows], na.rm = TRUE)
    }
  }
  
  data$ch4_background = ch4_background
  data$ch4_criteria   = ch4_criteria
  
  return(data)    
  
}

Background_nox = function(data) {
  t = 30
  
  nox_background = rep(NA, nrow(data)) 
  nox_criteria = rep(NA, nrow(data))
  
  data$date = as.POSIXct(data$date)
  
  for (i in 1:nrow(data)) {
    current_date = data$date[i]
    
    time_window_start = current_date - as.difftime(t, units = "secs")
    time_window_end = current_date + as.difftime(t, units = "secs")
    
    window_rows = which(data$date >= time_window_start & data$date <= time_window_end)
    
    if (length(window_rows) > 0) {
      nox_background[i] = mean(data$nox[window_rows], na.rm = TRUE)
      nox_criteria[i] = 2*sd(data$nox[window_rows], na.rm = TRUE)
    }
  }
  
  data$nox_background = nox_background
  data$nox_criteria   = nox_criteria
  
  return(data)    
  
}

Background_C2H6 = function(data) {
  t = 30
  
  C2H6_background = rep(NA, nrow(data))
  C2H6_criteria = rep(NA, nrow(data))
  
  data$date = as.POSIXct(data$date)
  
  for (i in 1:nrow(data)) {
    current_date = data$date[i]
    
    time_window_start = current_date - as.difftime(t, units = "secs")
    time_window_end = current_date + as.difftime(t, units = "secs")
    
    window_rows = which(data$date >= time_window_start & data$date <= time_window_end)
    
    if (length(window_rows) > 0) {
      C2H6_background[i] = mean(data$C2H6[window_rows], na.rm = TRUE)
      C2H6_criteria[i] = 2*sd(data$C2H6[window_rows], na.rm = TRUE)
    }
  }
  
  data$C2H6_background = C2H6_background
  data$C2H6_criteria   = C2H6_criteria
  
  return(data)    
  
}

haversine_distance = function(lat1, lon1, lat2, lon2) {
  R = 6371000  # Earth radius in meters
  
  dlat = (lat2 - lat1) * pi / 180
  dlon = (lon2 - lon1) * pi / 180
  
  a = sin(dlat/2) * sin(dlat/2) + cos(lat1 * pi / 180) * cos(lat2 * pi / 180) * sin(dlon/2) * sin(dlon/2)
  c = 2 * atan2(sqrt(a), sqrt(1-a))
  
  distance = R * c
  return(distance)
}


# Read data ---------------------------------------------------------------
C362 = read.csv(file = "G:/My Drive/PhD/R Projects/PTECE Flaring/PTECE Flaring/Data/C362_full_data.csv")

#Divided into individual runs
{
C362_Run1 = C362 %>% 
  filter(Time >= as_hms("11:50:40") & Time <= as_hms("11:56:21")) %>% 
  mutate(Run_no. = 1) 

C362_Run2 = C362 %>%  
  filter(Time >= as_hms("11:59:52") & Time <= as_hms("12:04:23")) %>% 
  mutate(Run_no. = 2) 

C362_Run3 = C362 %>%  
  filter(Time >= as_hms("12:08:09") & Time <= as_hms("12:10:33")) %>% 
  mutate(Run_no. = 3)

C362_Run4 = C362 %>%  
  filter(Time >= as_hms("12:13:20") & Time <= as_hms("12:16:08")) %>% 
  mutate(Run_no. = 4) 

C362_Run5 = C362 %>%  
  filter(Time >= as_hms("12:19:02") & Time <= as_hms("12:21:18")) %>% 
  mutate(Run_no. = 5)

C362_Run6 = C362 %>%  
  filter(Time >= as_hms("12:23:44") & Time <= as_hms("12:26:12")) %>%
  mutate(Run_no. = 6) 

C362_Run7 = C362 %>% 
  filter(Time >= as_hms("12:28:23") & Time <= as_hms("12:30:19")) %>% 
  mutate(Run_no. = 7)

C362_Run8 = C362 %>%  
  filter(Time >= as_hms("12:32:36") & Time <= as_hms("12:35:03")) %>% 
  mutate(Run_no. = 8)

C362_Run9 = C362 %>%  
  filter(Time >= as_hms("12:37:14") & Time <= as_hms("12:40:07")) %>% 
  mutate(Run_no. = 9)

C362_Run10 = C362 %>%  
  filter(Time >= as_hms("12:43:41") & Time <= as_hms("12:46:00")) %>% 
  mutate(Run_no. = 10)

C362_Run11 = C362 %>%  
  filter(Time >= as_hms("12:49:00") & Time <= as_hms("12:51:20")) %>% 
  mutate(Run_no. = 11) 

C362_Run12 = C362 %>%  
  filter(Time >= as_hms("12:54:19") & Time <= as_hms("12:57:20")) %>% 
  mutate(Run_no. = 12) 

C362_Run13 = C362 %>%  
  filter(Time >= as_hms("12:59:55") & Time <= as_hms("13:01:31")) %>% 
  mutate(Run_no. = 13) 

C362_annotated = bind_rows(C362_Run1, C362_Run2, C362_Run3, C362_Run4,
                            C362_Run5, C362_Run6, C362_Run7, C362_Run8,
                            C362_Run9, C362_Run10, C362_Run11, C362_Run12,
                            C362_Run13)
}



# Peak identification -----------------------------------------------------

C362_peaks = C362_annotated %>% 
  mutate(date = Time) %>% 
  group_by(Run_no.) %>% 
  Background_ch4() %>% 
  Background_CO2() %>% 
  Background_nox() %>% 
  Background_C2H6() %>% 
  mutate(ch4_enhancement = ch4_ppb - ch4_background,
         co2_enhancement = co2_ppm - co2_background,
         nox_enhancement = nox - nox_background,
         c2h6_enhancement = C2H6 - C2H6_background) %>% 
  mutate(ch4_ppb = if_else(ch4_enhancement < ch4_criteria, NaN, ch4_ppb),
         co2_ppm = if_else(co2_enhancement < co2_criteria, NaN, co2_ppm),
         nox = if_else(nox_enhancement < nox_criteria, NaN, nox),
         C2H6 = if_else(c2h6_enhancement < C2H6_criteria, NaN, C2H6)) %>% 
  tibble() 


# Peak aligning ------------------------------------------------------

#Run 2
{
  C362Run2.1_nox = C362_peaks %>% 
    filter(Run_no. == 2) %>% 
    select(c(LON_GIN, nox, nox_enhancement, Run_no.)) %>% 
    filter(LON_GIN > -2.535 & LON_GIN < -2.530) %>% 
    na.omit() %>% 
    tibble()
  
  C362Run2.1_CO2 = C362_peaks %>% 
    filter(Run_no. == 2) %>% 
    select(c(LON_GIN, co2_ppm, co2_enhancement, Run_no.)) %>% 
    filter(LON_GIN > -2.535 & LON_GIN < -2.530) %>% 
    na.omit() %>% 
    mutate(LON_GIN_diff = C362Run2.1_nox$LON_GIN[which.max(C362Run2.1_nox$nox)] - LON_GIN[which.max(co2_ppm)],
           LON_GIN = LON_GIN + LON_GIN_diff) %>%
    select(-LON_GIN_diff) %>%  
    tibble()
  
  
  C362Run2.1_ch4 = C362_peaks %>% 
    filter(Run_no. == 2) %>% 
    select(c(LON_GIN, ch4_ppb, ch4_enhancement, Run_no.)) %>% 
    filter(LON_GIN > -2.535 & LON_GIN < -2.530) %>% 
    na.omit() %>% 
    mutate(LON_GIN_diff = C362Run2.1_nox$LON_GIN[which.max(C362Run2.1_nox$nox)] - LON_GIN[which.max(ch4_ppb)],
           LON_GIN = LON_GIN + LON_GIN_diff) %>% 
    select(-LON_GIN_diff) %>% 
    tibble()
  
  
  C362Run2.1_peakstimealligned =  full_join(C362Run2.1_CO2, C362Run2.1_nox) %>% 
    full_join(C362Run2.1_ch4) %>% 
    mutate(peak = 2)
}

#Run 2
{
  C362Run2.2_nox = C362_peaks %>% 
    filter(Run_no. == 2) %>% 
    select(c(LON_GIN, nox, nox_enhancement, Run_no.)) %>% 
    filter(LON_GIN > -2.530 & LON_GIN < -2.5275) %>% 
    na.omit() %>% 
    tibble()
  
  C362Run2.2_CO2 = C362_peaks %>% 
    filter(Run_no. == 2) %>% 
    select(c(LON_GIN, co2_ppm, co2_enhancement, Run_no.)) %>% 
    filter(LON_GIN > -2.530 & LON_GIN < -2.5275) %>% 
    na.omit() %>% 
    mutate(LON_GIN_diff = C362Run2.2_nox$LON_GIN[which.max(C362Run2.2_nox$nox)] - LON_GIN[which.max(co2_ppm)],
           LON_GIN = LON_GIN + LON_GIN_diff) %>%
    select(-LON_GIN_diff) %>%  
    tibble()
  
  
  C362Run2.2_ch4 = C362_peaks %>% 
    filter(Run_no. == 2) %>% 
    select(c(LON_GIN, ch4_ppb, ch4_enhancement, Run_no.)) %>% 
    filter(LON_GIN > -2.530 & LON_GIN < -2.5275) %>% 
    na.omit() %>% 
    mutate(LON_GIN_diff = C362Run2.2_nox$LON_GIN[which.max(C362Run2.2_nox$nox)] - LON_GIN[which.max(ch4_ppb)],
           LON_GIN = LON_GIN + LON_GIN_diff) %>% 
    select(-LON_GIN_diff) %>% 
    tibble()
  
  
  C362Run2.2_peakstimealligned =  full_join(C362Run2.2_CO2, C362Run2.2_nox) %>% 
    full_join(C362Run2.2_ch4) %>% 
    mutate(peak = 2.2)
}

#Run 3
{
  C362Run3_nox = C362_peaks %>% 
    filter(Run_no. == 3) %>% 
    select(c(LON_GIN, nox, nox_enhancement, Run_no.)) %>% 
    filter(LON_GIN > -2.530 & LON_GIN < -2.52) %>% 
    na.omit() %>% 
    tibble()
  
  C362Run3_CO2 = C362_peaks %>% 
    filter(Run_no. == 3) %>% 
    select(c(LON_GIN, co2_ppm, co2_enhancement, Run_no.)) %>% 
    filter(LON_GIN > -2.530 & LON_GIN < -2.52) %>% 
    na.omit() %>% 
    mutate(LON_GIN_diff = C362Run3_nox$LON_GIN[which.max(C362Run3_nox$nox)] - LON_GIN[which.max(co2_ppm)],
           LON_GIN = LON_GIN + LON_GIN_diff) %>%
    select(-LON_GIN_diff) %>%  
    tibble()
  
  
  C362Run3_ch4 = C362_peaks %>% 
    filter(Run_no. == 3) %>% 
    select(c(LON_GIN, ch4_ppb, ch4_enhancement, Run_no.)) %>% 
    filter(LON_GIN > -2.530 & LON_GIN < -2.52) %>% 
    na.omit() %>% 
    mutate(LON_GIN_diff = C362Run3_nox$LON_GIN[which.max(C362Run3_nox$nox)] - LON_GIN[which.max(ch4_ppb)],
           LON_GIN = LON_GIN + LON_GIN_diff) %>% 
    select(-LON_GIN_diff) %>% 
    tibble()
  
  
  C362Run3_peakstimealligned =  full_join(C362Run3_CO2, C362Run3_nox) %>% 
    full_join(C362Run3_ch4)%>% 
    mutate(peak = 3) 
}

#Run 4
{
  C362Run4_nox = C362_peaks %>% 
    filter(Run_no. == 4) %>% 
    select(c(LON_GIN, nox, nox_enhancement, Run_no.)) %>% 
    filter(LON_GIN > -2.55 & LON_GIN < -2.525) %>% 
    na.omit() %>% 
    tibble()
  
  C362Run4_CO2 = C362_peaks %>% 
    filter(Run_no. == 4) %>% 
    select(c(LON_GIN, co2_ppm, co2_enhancement, Run_no.)) %>% 
    filter(LON_GIN > -2.55 & LON_GIN < -2.525) %>% 
    na.omit() %>% 
    mutate(LON_GIN_diff = C362Run4_nox$LON_GIN[which.max(C362Run4_nox$nox)] - LON_GIN[which.max(co2_ppm)],
           LON_GIN = LON_GIN + LON_GIN_diff) %>%
    select(-LON_GIN_diff) %>%  
    tibble()
  
  C362Run4_peakstimealligned =  full_join(C362Run4_CO2, C362Run4_nox)%>% 
    mutate(peak = 4)
}

#Run 5
{
  C362Run5_nox = C362_peaks %>% 
    filter(Run_no. == 5) %>% 
    select(c(LON_GIN, nox, nox_enhancement, Run_no.)) %>% 
    filter(LON_GIN > -2.55 & LON_GIN < -2.525) %>% 
    na.omit() %>% 
    tibble()
  
  C362Run5_CO2 = C362_peaks %>% 
    filter(Run_no. == 5) %>% 
    select(c(LON_GIN, co2_ppm, co2_enhancement, Run_no.)) %>% 
    filter(LON_GIN > -2.55 & LON_GIN < -2.525) %>% 
    na.omit() %>% 
    mutate(LON_GIN_diff = C362Run5_nox$LON_GIN[which.max(C362Run5_nox$nox)] - LON_GIN[which.max(co2_ppm)],
           LON_GIN = LON_GIN + LON_GIN_diff) %>%
    select(-LON_GIN_diff) %>%  
    tibble()
  
  C362Run5_c2h6 = C362_peaks %>% 
    filter(Run_no. == 5) %>% 
    select(c(LON_GIN, C2H6, c2h6_enhancement,  Run_no.)) %>% 
    filter(LON_GIN > -2.55 & LON_GIN < -2.525) %>% 
    na.omit() %>% 
    mutate(LON_GIN_diff = C362Run5_nox$LON_GIN[which.max(C362Run5_nox$nox)] - LON_GIN[which.max(C2H6)],
           LON_GIN = LON_GIN + LON_GIN_diff) %>% 
    select(-LON_GIN_diff) %>% 
    tibble()
  
  C362Run5_peakstimealligned =  full_join(C362Run5_CO2, C362Run5_nox) %>% 
    full_join(C362Run5_c2h6)%>% 
    mutate(peak = 5)
}

#Run 6
{
  C362Run6_nox = C362_peaks %>% 
    filter(Run_no. == 6) %>% 
    select(c(LON_GIN, nox, nox_enhancement, Run_no.)) %>% 
    filter(LON_GIN > -2.535 & LON_GIN < -2.530) %>% 
    na.omit() %>% 
    tibble()
  
  C362Run6_CO2 = C362_peaks %>% 
    filter(Run_no. == 6) %>% 
    select(c(LON_GIN, co2_ppm, co2_enhancement, Run_no.)) %>% 
    filter(LON_GIN > -2.535 & LON_GIN < -2.530) %>% 
    na.omit() %>% 
    mutate(LON_GIN_diff = C362Run6_nox$LON_GIN[which.max(C362Run6_nox$nox)] - LON_GIN[which.max(co2_ppm)],
           LON_GIN = LON_GIN + LON_GIN_diff) %>%
    select(-LON_GIN_diff) %>%  
    tibble()
  
  C362Run6_ch4 = C362_peaks %>% 
    filter(Run_no. == 6) %>% 
    select(c(LON_GIN, ch4_ppb, ch4_enhancement, Run_no.)) %>% 
    filter(LON_GIN > -2.535 & LON_GIN < -2.530) %>% 
    na.omit() %>% 
    mutate(LON_GIN_diff = C362Run6_nox$LON_GIN[which.max(C362Run6_nox$nox)] - LON_GIN[which.max(ch4_ppb)],
           LON_GIN = LON_GIN + LON_GIN_diff) %>% 
    select(-LON_GIN_diff) %>% 
    tibble()
  
  C362Run6_c2h6 = C362_peaks %>% 
    filter(Run_no. == 6) %>% 
    select(c(LON_GIN, C2H6, c2h6_enhancement,  Run_no.)) %>%
    filter(LON_GIN > -2.535 & LON_GIN < -2.525) %>% 
    na.omit() %>% 
    mutate(LON_GIN_diff = C362Run6_nox$LON_GIN[which.max(C362Run6_nox$nox)] - LON_GIN[which.max(C2H6)],
           LON_GIN = LON_GIN + LON_GIN_diff) %>% 
    select(-LON_GIN_diff) %>% 
    tibble()
  
  C362Run6_peakstimealligned =  full_join(C362Run6_CO2, C362Run6_nox) %>% 
    full_join(C362Run6_c2h6) %>% 
    full_join(C362Run6_ch4)%>% 
    mutate(peak = 6)
}

#Run 7
{
  C362Run7_nox = C362_peaks %>% 
    filter(Run_no. == 7) %>% 
    select(c(LON_GIN, nox, nox_enhancement, Run_no.)) %>% 
    filter(LON_GIN > -2.5325 & LON_GIN < -2.5275) %>% 
    na.omit() %>% 
    tibble()
  
  C362Run7_CO2 = C362_peaks %>% 
    filter(Run_no. == 7) %>% 
    select(c(LON_GIN, co2_ppm, co2_enhancement, Run_no.)) %>% 
    filter(LON_GIN > -2.5325 & LON_GIN < -2.5275) %>% 
    na.omit() %>% 
    mutate(LON_GIN_diff = C362Run7_nox$LON_GIN[which.max(C362Run7_nox$nox)] - LON_GIN[which.max(co2_ppm)],
           LON_GIN = LON_GIN + LON_GIN_diff) %>%
    select(-LON_GIN_diff) %>%  
    tibble()
  
  C362Run7_ch4 = C362_peaks %>% 
    filter(Run_no. == 7) %>% 
    select(c(LON_GIN, ch4_ppb, ch4_enhancement, Run_no.)) %>% 
    filter(LON_GIN > -2.5325 & LON_GIN < -2.5275) %>% 
    na.omit() %>% 
    mutate(LON_GIN_diff = C362Run7_nox$LON_GIN[which.max(C362Run7_nox$nox)] - LON_GIN[which.max(ch4_ppb)],
           LON_GIN = LON_GIN + LON_GIN_diff) %>% 
    select(-LON_GIN_diff) %>% 
    tibble()
  
  C362Run7_c2h6 = C362_peaks %>% 
    filter(Run_no. == 7) %>% 
    select(c(LON_GIN, C2H6, c2h6_enhancement,  Run_no.)) %>%
    filter(LON_GIN > -2.5325 & LON_GIN < -2.5275) %>% 
    na.omit() %>% 
    mutate(LON_GIN_diff = C362Run7_nox$LON_GIN[which.max(C362Run7_nox$nox)] - LON_GIN[which.max(C2H6)],
           LON_GIN = LON_GIN + LON_GIN_diff) %>% 
    select(-LON_GIN_diff) %>% 
    tibble()
  
  C362Run7_peakstimealligned =  full_join(C362Run7_CO2, C362Run7_nox) %>% 
    full_join(C362Run7_c2h6) %>% 
    full_join(C362Run7_ch4)%>% 
    mutate(peak = 7)
}

#Run 8
{
  C362Run8_nox = C362_peaks %>% 
    filter(Run_no. == 8) %>% 
    select(c(LON_GIN, nox, nox_enhancement, Run_no.)) %>% 
    filter(LON_GIN > -2.5325 & LON_GIN < -2.5275) %>% 
    na.omit() %>% 
    tibble()
  
  C362Run8_CO2 = C362_peaks %>% 
    filter(Run_no. == 8) %>% 
    select(c(LON_GIN, co2_ppm, co2_enhancement, Run_no.)) %>% 
    filter(LON_GIN > -2.5325 & LON_GIN < -2.5285) %>% 
    na.omit() %>% 
    mutate(LON_GIN_diff = C362Run8_nox$LON_GIN[which.max(C362Run8_nox$nox)] - LON_GIN[which.max(co2_ppm)],
           LON_GIN = LON_GIN + LON_GIN_diff) %>%
    select(-LON_GIN_diff) %>%  
    tibble()
  
  
  C362Run8_peakstimealligned =  full_join(C362Run8_CO2, C362Run8_nox)%>% 
    mutate(peak = 8)
}

#Run 9
{
  C362Run9_nox = C362_peaks %>% 
    filter(Run_no. == 9) %>% 
    select(c(LON_GIN, nox, nox_enhancement, Run_no.)) %>% 
    filter(LON_GIN > -2.55 & LON_GIN < -2.525) %>% 
    na.omit() %>% 
    tibble()
  
  C362Run9_CO2 = C362_peaks %>% 
    filter(Run_no. == 9) %>% 
    select(c(LON_GIN, co2_ppm, co2_enhancement, Run_no.)) %>% 
    filter(LON_GIN > -2.55 & LON_GIN < -2.525) %>% 
    na.omit() %>% 
    mutate(LON_GIN_diff = C362Run9_nox$LON_GIN[which.max(C362Run9_nox$nox)] - LON_GIN[which.max(co2_ppm)],
           LON_GIN = LON_GIN + LON_GIN_diff) %>%
    select(-LON_GIN_diff) %>%  
    tibble()
  
  C362Run9_c2h6 = C362_peaks %>% 
    filter(Run_no. == 9) %>% 
    select(c(LON_GIN, C2H6, c2h6_enhancement,  Run_no.)) %>%
    filter(LON_GIN > -2.55 & LON_GIN < -2.525) %>% 
    na.omit() %>% 
    mutate(LON_GIN_diff = C362Run9_nox$LON_GIN[which.max(C362Run9_nox$nox)] - LON_GIN[which.max(C2H6)],
           LON_GIN = LON_GIN + LON_GIN_diff) %>% 
    select(-LON_GIN_diff) %>% 
    tibble()
  
  C362Run9_peakstimealligned =  full_join(C362Run9_CO2, C362Run9_nox) %>% 
    full_join(C362Run9_c2h6) %>% 
    mutate(peak = 9)
}

#Run 10
{
  C362Run10_nox = C362_peaks %>% 
    filter(Run_no. == 10) %>% 
    select(c(LON_GIN, nox, nox_enhancement, Run_no.)) %>% 
    filter(LON_GIN > -2.538 & LON_GIN < -2.536) %>% 
    na.omit() %>% 
    tibble()
  
  C362Run10_CO2 = C362_peaks %>% 
    filter(Run_no. == 10) %>% 
    select(c(LON_GIN, co2_ppm, co2_enhancement, Run_no.)) %>% 
    filter(LON_GIN > -2.538 & LON_GIN < -2.536) %>% 
    na.omit() %>% 
    mutate(LON_GIN_diff = C362Run10_nox$LON_GIN[which.max(C362Run10_nox$nox)] - LON_GIN[which.max(co2_ppm)],
           LON_GIN = LON_GIN + LON_GIN_diff) %>%
    select(-LON_GIN_diff) %>%  
    tibble()
  
  C362Run10_ch4 = C362_peaks %>% 
    filter(Run_no. == 10) %>% 
    select(c(LON_GIN, ch4_ppb, ch4_enhancement, Run_no.)) %>% 
    filter(LON_GIN > -2.538 & LON_GIN < -2.536) %>% 
    na.omit() %>% 
    mutate(LON_GIN_diff = C362Run10_nox$LON_GIN[which.max(C362Run10_nox$nox)] - LON_GIN[which.max(ch4_ppb)],
           LON_GIN = LON_GIN + LON_GIN_diff) %>% 
    select(-LON_GIN_diff) %>% 
    tibble()
  
  C362Run10_c2h6 = C362_peaks %>% 
    filter(Run_no. == 10) %>% 
    select(c(LON_GIN, C2H6, c2h6_enhancement,  Run_no.)) %>%
    filter(LON_GIN > -2.538 & LON_GIN < -2.536) %>% 
    na.omit() %>% 
    mutate(LON_GIN_diff = C362Run10_nox$LON_GIN[which.max(C362Run10_nox$nox)] - LON_GIN[which.max(C2H6)],
           LON_GIN = LON_GIN + LON_GIN_diff) %>% 
    select(-LON_GIN_diff) %>% 
    tibble()
  
  C362Run10_peakstimealligned =  full_join(C362Run10_CO2, C362Run10_nox) %>% 
    full_join(C362Run10_c2h6) %>% 
    full_join(C362Run10_ch4)%>% 
    mutate(peak = 10)
}

#Run 10
{
  C362Run10.2_nox = C362_peaks %>% 
    filter(Run_no. == 10) %>% 
    select(c(LON_GIN, nox, nox_enhancement, Run_no.)) %>% 
    filter(LON_GIN > -2.536 & LON_GIN < -2.533) %>% 
    na.omit() %>% 
    tibble()
  
  C362Run10.2_CO2 = C362_peaks %>% 
    filter(Run_no. == 10) %>% 
    select(c(LON_GIN, co2_ppm, co2_enhancement, Run_no.)) %>% 
    filter(LON_GIN > -2.536 & LON_GIN < -2.533) %>% 
    na.omit() %>% 
    mutate(LON_GIN_diff = C362Run10.2_nox$LON_GIN[which.max(C362Run10.2_nox$nox)] - LON_GIN[which.max(co2_ppm)],
           LON_GIN = LON_GIN + LON_GIN_diff) %>%
    select(-LON_GIN_diff) %>%  
    tibble()
  
  C362Run10.2_ch4 = C362_peaks %>% 
    filter(Run_no. == 10) %>% 
    select(c(LON_GIN, ch4_ppb, ch4_enhancement, Run_no.)) %>% 
    filter(LON_GIN > -2.536 & LON_GIN < -2.533) %>% 
    na.omit() %>% 
    mutate(LON_GIN_diff = C362Run10.2_nox$LON_GIN[which.max(C362Run10.2_nox$nox)] - LON_GIN[which.max(ch4_ppb)],
           LON_GIN = LON_GIN + LON_GIN_diff) %>% 
    select(-LON_GIN_diff) %>% 
    tibble()
  
  C362Run10.2_c2h6 = C362_peaks %>% 
    filter(Run_no. == 10) %>% 
    select(c(LON_GIN, C2H6, c2h6_enhancement,  Run_no.)) %>%
    filter(LON_GIN > -2.536 & LON_GIN < -2.533) %>% 
    na.omit() %>% 
    mutate(LON_GIN_diff = C362Run10.2_nox$LON_GIN[which.max(C362Run10.2_nox$nox)] - LON_GIN[which.max(C2H6)],
           LON_GIN = LON_GIN + LON_GIN_diff) %>% 
    select(-LON_GIN_diff) %>% 
    tibble()
  
  C362Run10.2_peakstimealligned =  full_join(C362Run10.2_CO2, C362Run10.2_nox) %>% 
    full_join(C362Run10.2_c2h6) %>% 
    full_join(C362Run10.2_ch4)%>% 
    mutate(peak = 10.2)
}

#Run 11
{
  C362Run11_nox = C362_peaks %>% 
    filter(Run_no. == 11) %>% 
    select(c(LON_GIN, nox, nox_enhancement, Run_no.)) %>% 
    filter(LON_GIN > -2.5375 & LON_GIN < -2.536) %>% 
    na.omit() %>% 
    tibble()
  
  C362Run11_CO2 = C362_peaks %>% 
    filter(Run_no. == 11) %>% 
    select(c(LON_GIN, co2_ppm, co2_enhancement, Run_no.)) %>% 
    filter(LON_GIN > -2.5375 & LON_GIN < -2.536) %>% 
    na.omit() %>% 
    mutate(LON_GIN_diff = C362Run11_nox$LON_GIN[which.max(C362Run11_nox$nox)] - LON_GIN[which.max(co2_ppm)],
           LON_GIN = LON_GIN + LON_GIN_diff) %>%
    select(-LON_GIN_diff) %>%  
    tibble()
  
  C362Run11_ch4 = C362_peaks %>% 
    filter(Run_no. == 11) %>% 
    select(c(LON_GIN, ch4_ppb, ch4_enhancement, Run_no.)) %>% 
    filter(LON_GIN > -2.5375 & LON_GIN < -2.536) %>% 
    na.omit() %>% 
    mutate(LON_GIN_diff = C362Run11_nox$LON_GIN[which.max(C362Run11_nox$nox)] - LON_GIN[which.max(ch4_ppb)],
           LON_GIN = LON_GIN + LON_GIN_diff) %>% 
    select(-LON_GIN_diff) %>% 
    tibble()
  
  C362Run11_c2h6 = C362_peaks %>% 
    filter(Run_no. == 11) %>% 
    select(c(LON_GIN, C2H6, c2h6_enhancement,  Run_no.)) %>%
    filter(LON_GIN > -2.535) %>% 
    na.omit() %>% 
    mutate(LON_GIN_diff = C362Run11_nox$LON_GIN[which.max(C362Run11_nox$nox)] - LON_GIN[which.max(C2H6)],
           LON_GIN = LON_GIN + LON_GIN_diff) %>% 
    select(-LON_GIN_diff) %>% 
    tibble()
  
  C362Run11_peakstimealligned =  full_join(C362Run11_CO2, C362Run11_nox) %>% 
    full_join(C362Run11_c2h6) %>% 
    full_join(C362Run11_ch4) %>% 
    mutate(peak = 11)
}

#Run 11
{
  C362Run11.2_nox = C362_peaks %>% 
    filter(Run_no. == 11) %>% 
    select(c(LON_GIN, nox, nox_enhancement, Run_no.)) %>% 
    filter(LON_GIN > -2.536 & LON_GIN < -2.5325) %>% 
    na.omit() %>% 
    tibble()
  
  C362Run11.2_CO2 = C362_peaks %>% 
    filter(Run_no. == 11) %>% 
    select(c(LON_GIN, co2_ppm, co2_enhancement, Run_no.)) %>% 
    filter(LON_GIN > -2.536 & LON_GIN < -2.5325) %>% 
    na.omit() %>% 
    mutate(LON_GIN_diff = C362Run11.2_nox$LON_GIN[which.max(C362Run11.2_nox$nox)] - LON_GIN[which.max(co2_ppm)],
           LON_GIN = LON_GIN + LON_GIN_diff) %>%
    select(-LON_GIN_diff) %>%  
    tibble()
  
  C362Run11.2_ch4 = C362_peaks %>% 
    filter(Run_no. == 11) %>% 
    select(c(LON_GIN, ch4_ppb, ch4_enhancement, Run_no.)) %>% 
    filter(LON_GIN > -2.536 & LON_GIN < -2.5325) %>% 
    na.omit() %>% 
    mutate(LON_GIN_diff = C362Run11.2_nox$LON_GIN[which.max(C362Run11.2_nox$nox)] - LON_GIN[which.max(ch4_ppb)],
           LON_GIN = LON_GIN + LON_GIN_diff) %>% 
    select(-LON_GIN_diff) %>% 
    tibble()
  
  C362Run11.2_c2h6 = C362_peaks %>% 
    filter(Run_no. == 11) %>% 
    select(c(LON_GIN, C2H6, c2h6_enhancement,  Run_no.)) %>%
    filter(LON_GIN > -2.536 & LON_GIN < -2.5325) %>% 
    na.omit() %>% 
    mutate(LON_GIN_diff = C362Run11.2_nox$LON_GIN[which.max(C362Run11.2_nox$nox)] - LON_GIN[which.max(C2H6)],
           LON_GIN = LON_GIN + LON_GIN_diff) %>% 
    select(-LON_GIN_diff) %>% 
    tibble()
  
  C362Run11.2_peakstimealligned =  full_join(C362Run11.2_CO2, C362Run11.2_nox) %>% 
    full_join(C362Run11.2_c2h6) %>% 
    full_join(C362Run11.2_ch4)%>% 
    mutate(peak = 11.2)
}

#Run 12
{
  C362Run12_nox = C362_peaks %>% 
    filter(Run_no. == 12) %>% 
    select(c(LON_GIN, nox, nox_enhancement, Run_no.)) %>% 
    filter(LON_GIN > -2.5375 & LON_GIN < -2.535) %>% 
    na.omit() %>% 
    tibble()
  
  C362Run12_CO2 = C362_peaks %>% 
    filter(Run_no. == 12) %>% 
    select(c(LON_GIN, co2_ppm, co2_enhancement, Run_no.)) %>% 
    filter(LON_GIN > -2.5375 & LON_GIN < -2.535) %>% 
    na.omit() %>% 
    mutate(LON_GIN_diff = C362Run12_nox$LON_GIN[which.max(C362Run12_nox$nox)] - LON_GIN[which.max(co2_ppm)],
           LON_GIN = LON_GIN + LON_GIN_diff) %>%
    select(-LON_GIN_diff) %>%  
    tibble()
  
  C362Run12_ch4 = C362_peaks %>% 
    filter(Run_no. == 12) %>% 
    select(c(LON_GIN, ch4_ppb, ch4_enhancement, Run_no.)) %>% 
    filter(LON_GIN > -2.5373 & LON_GIN < -2.535) %>% 
    na.omit() %>% 
    mutate(LON_GIN_diff = C362Run12_nox$LON_GIN[which.max(C362Run12_nox$nox)] - LON_GIN[which.max(ch4_ppb)],
           LON_GIN = LON_GIN + LON_GIN_diff) %>% 
    select(-LON_GIN_diff) %>% 
    tibble()
  
  C362Run12_c2h6 = C362_peaks %>% 
    filter(Run_no. == 12) %>% 
    select(c(LON_GIN, C2H6, c2h6_enhancement,  Run_no.)) %>%
    filter(LON_GIN > -2.5375 & LON_GIN < -2.535) %>% 
    na.omit() %>% 
    mutate(LON_GIN_diff = C362Run12_nox$LON_GIN[which.max(C362Run12_nox$nox)] - LON_GIN[which.max(C2H6)],
           LON_GIN = LON_GIN + LON_GIN_diff) %>% 
    select(-LON_GIN_diff) %>% 
    tibble()
  
  C362Run12_peakstimealligned =  full_join(C362Run12_CO2, C362Run12_nox) %>% 
    full_join(C362Run12_c2h6) %>% 
    full_join(C362Run12_ch4)%>% 
    mutate(peak = 12)
}

#Run 12
{
  C362Run12.2_nox = C362_peaks %>% 
    filter(Run_no. == 12) %>% 
    select(c(LON_GIN, nox, nox_enhancement, Run_no.)) %>% 
    filter(LON_GIN > -2.535 & LON_GIN < -2.5325) %>% 
    na.omit() %>% 
    tibble()
  
  C362Run12.2_CO2 = C362_peaks %>% 
    filter(Run_no. == 12) %>% 
    select(c(LON_GIN, co2_ppm, co2_enhancement, Run_no.)) %>% 
    filter(LON_GIN > -2.535 & LON_GIN < -2.5325) %>% 
    na.omit() %>% 
    mutate(LON_GIN_diff = C362Run12.2_nox$LON_GIN[which.max(C362Run12.2_nox$nox)] - LON_GIN[which.max(co2_ppm)],
           LON_GIN = LON_GIN + LON_GIN_diff) %>%
    select(-LON_GIN_diff) %>%  
    tibble()
  
  C362Run12.2_ch4 = C362_peaks %>% 
    filter(Run_no. == 12) %>% 
    select(c(LON_GIN, ch4_ppb, ch4_enhancement, Run_no.)) %>% 
    filter(LON_GIN > -2.535 & LON_GIN < -2.5325) %>% 
    na.omit() %>% 
    mutate(LON_GIN_diff = C362Run12.2_nox$LON_GIN[which.max(C362Run12.2_nox$nox)] - LON_GIN[which.max(ch4_ppb)],
           LON_GIN = LON_GIN + LON_GIN_diff) %>% 
    select(-LON_GIN_diff) %>% 
    tibble()
  
  C362Run12.2_c2h6 = C362_peaks %>% 
    filter(Run_no. == 12) %>% 
    select(c(LON_GIN, C2H6, c2h6_enhancement,  Run_no.)) %>%
    filter(LON_GIN > -2.535 & LON_GIN < -2.53) %>% 
    na.omit() %>% 
    mutate(LON_GIN_diff = C362Run12.2_nox$LON_GIN[which.max(C362Run12.2_nox$nox)] - LON_GIN[which.max(C2H6)],
           LON_GIN = LON_GIN + LON_GIN_diff) %>% 
    select(-LON_GIN_diff) %>% 
    tibble()
  
  C362Run12.2_peakstimealligned =  full_join(C362Run12.2_CO2, C362Run12.2_nox) %>% 
    full_join(C362Run12.2_c2h6) %>% 
    full_join(C362Run12.2_ch4)%>% 
    mutate(peak = 12.2)
}

#Run 13
{
  C362Run13_nox = C362_peaks %>% 
    filter(Run_no. == 13) %>% 
    select(c(LON_GIN, nox, nox_enhancement, Run_no.)) %>% 
    filter(LON_GIN > -2.54 & LON_GIN < -2.535) %>% 
    na.omit() %>% 
    tibble()
  
  C362Run13_CO2 = C362_peaks %>% 
    filter(Run_no. == 13) %>% 
    select(c(LON_GIN, co2_ppm, co2_enhancement, Run_no.)) %>% 
    filter(LON_GIN > -2.54 & LON_GIN < -2.535) %>% 
    na.omit() %>% 
    mutate(LON_GIN_diff = C362Run13_nox$LON_GIN[which.max(C362Run13_nox$nox)] - LON_GIN[which.max(co2_ppm)],
           LON_GIN = LON_GIN + LON_GIN_diff) %>%
    select(-LON_GIN_diff) %>%  
    tibble()
  
  C362Run13_ch4 = C362_peaks %>% 
    filter(Run_no. == 13) %>% 
    select(c(LON_GIN, ch4_ppb, ch4_enhancement, Run_no.)) %>% 
    filter(LON_GIN > -2.54 & LON_GIN < -2.535) %>% 
    na.omit() %>% 
    mutate(LON_GIN_diff = C362Run13_nox$LON_GIN[which.max(C362Run13_nox$nox)] - LON_GIN[which.max(ch4_ppb)],
           LON_GIN = LON_GIN + LON_GIN_diff) %>% 
    select(-LON_GIN_diff) %>% 
    tibble()
  
  C362Run13_c2h6 = C362_peaks %>% 
    filter(Run_no. == 13) %>% 
    select(c(LON_GIN, C2H6, c2h6_enhancement,  Run_no.)) %>%
    filter(LON_GIN > -2.545 & LON_GIN < -2.535) %>% 
    na.omit() %>% 
    mutate(LON_GIN_diff = C362Run13_nox$LON_GIN[which.max(C362Run13_nox$nox)] - LON_GIN[which.max(C2H6)],
           LON_GIN = LON_GIN + LON_GIN_diff) %>% 
    select(-LON_GIN_diff) %>% 
    tibble()
  
  C362Run13_peakstimealligned =  full_join(C362Run13_CO2, C362Run13_nox) %>% 
    full_join(C362Run13_c2h6) %>% 
    full_join(C362Run13_ch4)%>% 
    mutate(peak = 13)
}

#Run 13
{
  C362Run13a_nox = C362_peaks %>% 
    filter(Run_no. == 13) %>% 
    select(c(LON_GIN, nox, nox_enhancement, Run_no.)) %>% 
    filter(LON_GIN > -2.49 & LON_GIN < -2.487) %>% 
    na.omit() %>% 
    tibble()
  
  C362Run13a_CO2 = C362_peaks %>% 
    filter(Run_no. == 13) %>% 
    select(c(LON_GIN, co2_ppm, co2_enhancement, Run_no.)) %>% 
    filter(LON_GIN > -2.49 & LON_GIN < -2.487) %>% 
    na.omit() %>% 
    mutate(LON_GIN_diff = C362Run13a_nox$LON_GIN[which.max(C362Run13a_nox$nox)] - LON_GIN[which.max(co2_ppm)],
           LON_GIN = LON_GIN + LON_GIN_diff) %>%
    select(-LON_GIN_diff) %>%  
    tibble()
  
  C362Run13a_ch4 = C362_peaks %>% 
    filter(Run_no. == 13) %>% 
    select(c(LON_GIN, ch4_ppb, ch4_enhancement, Run_no.)) %>% 
    filter(LON_GIN > -2.49 & LON_GIN < -2.487) %>% 
    na.omit() %>% 
    mutate(LON_GIN_diff = C362Run13a_nox$LON_GIN[which.max(C362Run13a_nox$nox)] - LON_GIN[which.max(ch4_ppb)],
           LON_GIN = LON_GIN + LON_GIN_diff) %>% 
    select(-LON_GIN_diff) %>% 
    tibble()
  
  C362Run13a_c2h6 = C362_peaks %>% 
    filter(Run_no. == 13) %>% 
    select(c(LON_GIN, C2H6, c2h6_enhancement,  Run_no.)) %>%
    filter(LON_GIN > -2.486) %>% 
    na.omit() %>% 
    mutate(LON_GIN_diff = C362Run13a_nox$LON_GIN[which.max(C362Run13a_nox$nox)] - LON_GIN[which.max(C2H6)],
           LON_GIN = LON_GIN + LON_GIN_diff) %>% 
    select(-LON_GIN_diff) %>% 
    tibble()
  
  C362Run13a_peaksRun13aalligned =  full_join(C362Run13a_CO2, C362Run13a_nox) %>% 
    full_join(C362Run13a_c2h6) %>% 
    full_join(C362Run13a_ch4)%>% 
    mutate(peak = 13.1)
}

#Run 13
{
  C362Run13b_nox = C362_peaks %>% 
    filter(Run_no. == 13) %>% 
    select(c(LON_GIN, nox, nox_enhancement, Run_no.)) %>% 
    filter(LON_GIN > -2.487 & LON_GIN < -2.484) %>% 
    na.omit() %>% 
    tibble()
  
  C362Run13b_CO2 = C362_peaks %>% 
    filter(Run_no. == 13) %>% 
    select(c(LON_GIN, co2_ppm, co2_enhancement, Run_no.)) %>% 
    filter(LON_GIN > -2.487 & LON_GIN < -2.484) %>% 
    na.omit() %>% 
    mutate(LON_GIN_diff = C362Run13b_nox$LON_GIN[which.max(C362Run13b_nox$nox)] - LON_GIN[which.max(co2_ppm)],
           LON_GIN = LON_GIN + LON_GIN_diff) %>%
    select(-LON_GIN_diff) %>%  
    tibble()
  
  
  C362Run13b_peaksRun13balligned =  full_join(C362Run13b_CO2, C362Run13b_nox) %>% 
    mutate(peak = 13.2)
}
LATS_LONS = C362_annotated %>% 
  select(c(LAT_GIN, LON_GIN)) %>% 
  na.omit()

deltaX_calc = function(LAT_GIN, LON_GIN) {
  haversine_distance(min(LAT_GIN, na.rm = TRUE), min(LON_GIN, na.rm = TRUE), 
                     max(LAT_GIN, na.rm = TRUE), max(LON_GIN, na.rm = TRUE))
}

C362_peakstimealligned = bind_rows(C362Run2.1_peakstimealligned, C362Run3_peakstimealligned,
                                    C362Run4_peakstimealligned, C362Run5_peakstimealligned,
                                    C362Run6_peakstimealligned, C362Run7_peakstimealligned,
                                    C362Run8_peakstimealligned, C362Run9_peakstimealligned,
                                    C362Run10_peakstimealligned, C362Run10.2_peakstimealligned,
                                    C362Run11_peakstimealligned, C362Run11.2_peakstimealligned,
                                    C362Run12_peakstimealligned, C362Run12.2_peakstimealligned,
                                    C362Run13_peakstimealligned, C362Run13a_peaksRun13aalligned,
                                    C362Run13b_peaksRun13balligned) %>% 
  left_join(LATS_LONS, by = "LON_GIN") %>% 
  group_by(peak) %>% 
  summarise(
    ER = (max(nox_enhancement, na.rm = TRUE)/1000000)/max(co2_enhancement, na.rm = TRUE),
    ER_C2H6 = ((max(c2h6_enhancement, na.rm = TRUE))/ max(ch4_enhancement, na.rm = TRUE)),
    deltaXCO2  = deltaX_calc(LAT_GIN[!is.na(co2_ppm)], LON_GIN[!is.na(co2_ppm)]),
    deltaXNOx  = deltaX_calc(LAT_GIN[!is.na(nox)],    LON_GIN[!is.na(nox)]),
    deltaXCH4  = deltaX_calc(LAT_GIN[!is.na(ch4_ppb)],LON_GIN[!is.na(ch4_ppb)]),
    deltaXC2H6 = deltaX_calc(LAT_GIN[!is.na(C2H6)],  LON_GIN[!is.na(C2H6)]),
    co2_enhancement = max(co2_enhancement, na.rm = TRUE),
    ch4_enhancement = max(ch4_enhancement, na.rm = TRUE)/1000,
    c2h6_enhancement = max(c2h6_enhancement, na.rm = TRUE)/1000,
    nox_enhancement = max(nox_enhancement, na.rm = TRUE)/1000000,
    Run_no. = mean(Run_no.)
  ) %>% 
  mutate(Flight = "C362")



# CO2 Flux of power generation ------------------------------------------------

Power_Gen_Signals = C362_peakstimealligned %>% 
  filter(ER > 0.009) %>% 
  filter(Run_no. != 12)

Met = C362_all_annotated %>% 
  filter(Run_no. %in% c(2,3,4,5,6,7,8,9,10,11,13)) %>% 
  group_by(Run_no.) %>% 
  mutate(
    TRCK_GIN = TRCK_GIN * (pi / 180),  # convert to radians
    crosswind = U_C * sin(TRCK_GIN) - V_C * cos(TRCK_GIN)
  ) %>% 
  summarise(
    U_C_sd = sd(crosswind, na.rm = TRUE),
    crosswind = mean(crosswind, na.rm = TRUE),
    p_sd   = sd(PS_RVSM, na.rm = TRUE) * 100,
    p      = mean(PS_RVSM, na.rm = TRUE) * 100,
    T_sd   = sd(TAT_DI_R, na.rm = TRUE),
    Temp   = mean(TAT_DI_R, na.rm = TRUE),
    .groups = "drop"
  )


Met$Run_no. = as.integer(Met$Run_no.)

Altitude = C362_annotated %>% 
  mutate(date = Time) %>% 
  group_by(Run_no.) %>% 
  Background_CO2() %>% 
  Background_ch4() %>% 
  Background_C2H6() %>% 
  mutate(co2_enhancement = co2_ppm - co2_background,
         ch4_enhancement = ch4_ppb - ch4_background,
         c2h6_enhancement = C2H6 - C2H6_background) %>% 
  summarise(CI_sd = sd(co2_enhancement, na.rm =T)/ 1e6,
            CIch4_sd = sd(ch4_enhancement, na.rm = T)/1e9,
            CIc2h6_sd = sd(c2h6_enhancement, na.rm = T)/1e9,
            min_ALT = min(ALT_GIN),
            max_ALT = max(ALT_GIN),
            ALT_sd = sd(ALT_GIN, na.rm = TRUE),
            ALT_GIN = mean(ALT_GIN)) %>% 
  filter(Run_no. %in% c(2,3,4,5,6,7,8,9,10,11,13)) %>% 
  mutate(Run_no. = as.integer(Run_no.))


Altitude = bind_rows(Altitude, tibble(Run_no. = c(100,200), ALT_GIN = c(93,409.3914)))%>% 
  arrange(ALT_GIN)%>% 
  mutate(
    lowerD = (ALT_GIN - lag(ALT_GIN)) / 2,
    upperD = (lead(ALT_GIN) - ALT_GIN) / 2,
    D = coalesce(lowerD, 0) + coalesce(upperD, 0)
  ) %>% 
  mutate(Run_no. = as.integer(Run_no.))



Calculate = left_join(Power_Gen_Signals, Altitude) %>% 
  left_join(Met)



Flux = Calculate %>% 
  group_by(Run_no.) %>% 
  mutate(deltaC_co2 = co2_enhancement/ 1e6,
         deltaC_ch4 = ch4_enhancement/ 1e6,
         deltaC_C2H6 = c2h6_enhancement/1e6,
         ideal_gas = ((p)/(8.314*Temp))
  ) %>%
  ungroup() %>% 
  mutate(Flux_CO2 = abs(deltaC_co2*ideal_gas*crosswind*deltaXCO2*D*0.044),
         Flux_ch4 = abs(deltaC_ch4*ideal_gas*crosswind*deltaXCH4*D*0.016),
         Flux_c2h6 = abs(deltaC_C2H6*ideal_gas*crosswind*deltaXC2H6*D*0.030),
         sigma = deltaXCO2 / (2 * sqrt(2 * log(2))),
         peak_area = deltaC_co2 * sigma * sqrt(2 * pi)) %>% 
  tibble() %>%
  mutate(Uncertainty_CO2 = Flux_CO2 * (sqrt((CI_sd/deltaC_co2)^2+(U_C_sd/crosswind)^2+(p_sd/p)^2 +(T_sd/Temp)^2+ (ALT_sd/D)^2)),
         Uncertainty_ch4 = Flux_ch4 * (sqrt((CIch4_sd/deltaC_ch4)^2+(U_C_sd/crosswind)^2+(p_sd/p)^2 +(T_sd/Temp)^2+ (ALT_sd/D)^2)),
         Uncertainty_c2h6 = Flux_c2h6 * (sqrt((CIc2h6_sd/deltaC_C2H6)^2+(U_C_sd/crosswind)^2+(p_sd/p)^2 +(T_sd/Temp)^2+ (ALT_sd/D)^2)))

#Predicted Power Gen Flux
sum(Flux$Flux_CO2)
sqrt(sum(Flux$Uncertainty_CO2^2))
sum(Flux$Flux_ch4, na.rm = T)
sqrt(sum(Flux$Uncertainty_ch4^2, na.rm = T))
sum(Flux$Flux_c2h6, na.rm = T)
sum(Flux$Uncertainty_c2h6, na.rm = T)


#Modelling flaring as a Gaussian
model = lm(Flux_CO2 ~ poly(ALT_GIN, 2, raw = TRUE), data = Flux)

coeffs = coef(model)


a = coeffs[3]  
b = coeffs[2]  


altitude_peak = -b / (2 * a)
altitude_peak

max_gaussian = altitude_peak+(altitude_peak-min(Altitude$min_ALT, na.rm = T))

current_gaussian = max(Altitude$max_ALT, na.rm = T)-min(Altitude$min_ALT, na.rm = T)
enhanced_gaussian = max_gaussian - min(Altitude$min_ALT, na.rm = T)

scaling_factor = enhanced_gaussian/current_gaussian

#Power Gen Flux after scaling
sum(Flux$Flux_CO2) * scaling_factor
sqrt(sum(Flux$Uncertainty_CO2^2)) * scaling_factor
sum(Flux$Flux_ch4, na.rm = T) * scaling_factor
sqrt(sum(Flux$Uncertainty_ch4^2, na.rm = T)) * scaling_factor
sum(Flux$Flux_c2h6, na.rm = T) * scaling_factor
sum(Flux$Uncertainty_c2h6, na.rm = T) * scaling_factor


# Flare Flux --------------------------------------------------------------
Flare_Signals = C362_peakstimealligned %>%
  filter(Run_no. %in% c(12,13)) %>% 
  filter(ER < 0.01)

Met = C362_all_annotated %>% 
  filter(Run_no. %in% c(12,13)) %>% 
  group_by(Run_no.) %>% 
  mutate(
    TRCK_GIN = TRCK_GIN * (pi / 180),  # convert to radians
    crosswind = U_C * sin(TRCK_GIN) - V_C * cos(TRCK_GIN)
  ) %>% 
  summarise(
    U_C_sd = sd(crosswind, na.rm = TRUE),
    crosswind = mean(crosswind, na.rm = TRUE),
    p_sd   = sd(PS_RVSM, na.rm = TRUE) * 100,
    p      = mean(PS_RVSM, na.rm = TRUE) * 100,
    T_sd   = sd(TAT_DI_R, na.rm = TRUE),
    Temp   = mean(TAT_DI_R, na.rm = TRUE),
    .groups = "drop"
  )


Met$Run_no. = as.integer(Met$Run_no.)

Altitude = C362_annotated %>% 
  mutate(date = Time) %>% 
  group_by(Run_no.) %>% 
  Background_CO2() %>% 
  Background_ch4() %>% 
  Background_C2H6() %>% 
  mutate(co2_enhancement = co2_ppm - co2_background,
         ch4_enhancement = ch4_ppb - ch4_background,
         c2h6_enhancement = C2H6 - C2H6_background) %>% 
  summarise(CI_sd = sd(co2_enhancement, na.rm =T)/ 1e6,
            CIch4_sd = sd(ch4_enhancement, na.rm = T)/1e9,
            CIc2h6_sd = sd(c2h6_enhancement, na.rm = T)/1e9,
            min_ALT = min(ALT_GIN),
            max_ALT = max(ALT_GIN),
            ALT_sd = sd(ALT_GIN, na.rm = TRUE),
            ALT_GIN = mean(ALT_GIN)) %>% 
  filter(Run_no. %in% c(3,12,13)) %>% 
  mutate(Run_no. = as.integer(Run_no.))


Altitude = bind_rows(Altitude, tibble(Run_no. = c(100), ALT_GIN = c(93.937)))%>% 
  arrange(ALT_GIN)%>% 
  mutate(
    lowerD = (ALT_GIN - lag(ALT_GIN)) / 2,
    upperD = (lead(ALT_GIN) - ALT_GIN) / 2,
    D = coalesce(lowerD, 0) + coalesce(upperD, 0)
  ) %>% 
  mutate(Run_no. = as.integer(Run_no.))



Calculate = left_join(Flare_Signals, Altitude) %>% 
  left_join(Met) %>% 
  filter(peak != 13.2)

Flux = Calculate %>% 
  group_by(Run_no.) %>% 
  mutate(deltaC_co2 = co2_enhancement/ 1e6,
         deltaC_ch4 = ch4_enhancement/ 1e6,
         deltaC_C2H6 = c2h6_enhancement/1e6,
         ideal_gas = ((p)/(8.314*Temp))
  ) %>%
  ungroup() %>% 
  mutate(Flux_CO2 = abs(deltaC_co2*ideal_gas*crosswind*deltaXCO2*D*0.044),
         Flux_ch4 = abs(deltaC_ch4*ideal_gas*crosswind*deltaXCH4*D*0.016),
         Flux_c2h6 = abs(deltaC_C2H6*ideal_gas*crosswind*deltaXC2H6*D*0.030),
         sigma = deltaXCO2 / (2 * sqrt(2 * log(2))),
         peak_area = deltaC_co2 * sigma * sqrt(2 * pi)) %>% 
  tibble() %>%
  mutate(Uncertainty_CO2 = Flux_CO2 * (sqrt((CI_sd/deltaC_co2)^2+(U_C_sd/crosswind)^2+(p_sd/p)^2 +(T_sd/Temp)^2+ (ALT_sd/D)^2)),
         Uncertainty_ch4 = Flux_ch4 * (sqrt((CIch4_sd/deltaC_ch4)^2+(U_C_sd/crosswind)^2+(p_sd/p)^2 +(T_sd/Temp)^2+ (ALT_sd/D)^2)),
         Uncertainty_c2h6 = Flux_c2h6 * (sqrt((CIc2h6_sd/deltaC_C2H6)^2+(U_C_sd/crosswind)^2+(p_sd/p)^2 +(T_sd/Temp)^2+ (ALT_sd/D)^2)))

#Predicted Flaring Fluxes
sum(Flux$Flux_CO2)
sqrt(sum(Flux$Uncertainty_CO2^2))
sum(Flux$Flux_ch4, na.rm = T)
sqrt(sum(Flux$Uncertainty_ch4^2, na.rm = T))
sum(Flux$Flux_c2h6, na.rm = T)
sum(Flux$Uncertainty_c2h6, na.rm = T)
