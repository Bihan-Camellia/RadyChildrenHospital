patient_volume_hourly<-encounters_new %>% 
  mutate(hour=hour(ADT_ARRIVAL_DTTM)) %>% 
  group_by(ADT_ARRIVAL_DATE,hour) %>% 
  summarise(number=n()) 
  
