diagnose <- read_delim("/srv/rsdata/UCSD_ED_ENCOUNTER_DIAG.csv", ";", 
                       escape_double = FALSE, trim_ws = TRUE)
as.Date(diagnose$ADT_ARRIVAL_DTTM[1:10])
# no other timestamp variables require parsing
chiefComplaint <- read_delim('/srv/rsdata/UCSD_ED_CHIEF_COMPLAINT.csv', ';',
                             escape_double = FALSE, trim_ws = TRUE)

d <- diagnose %>%
  mutate(ADT_ARRIVAL_D = as.POSIXct(ADT_ARRIVAL_DTTM, format="%m/%d/%Y %H:%M"),
         ADT_DATE = date(ADT_ARRIVAL_D),
         ADT_WEEKDAYS = wday(ADT_ARRIVAL_D, label = T),
         ADT_HOUR = hour(ADT_ARRIVAL_D),
         ADT_MONTH = factor(month(ADT_ARRIVAL_D),
                            levels = c('1', '2', '3', '4', '5', '6', '7','8', '9', '10', '11', '12'),
                            labels = c('January','February','March','April','May','June','July',
                                       'August','September','October','November','December'))
  ) %>%
  group_by(ADT_HOUR, ADT_DATE, ADT_WEEKDAYS, ADT_MONTH) %>%
  summarize(bp = sum(BEHAVIORAL_JASON),
            n = n()) %>%
  ungroup()

dc <- chiefComplaint %>%
  mutate(ADT_ARRIVAL_D = as.POSIXct(ADT_ARRIVAL_DTTM, format="%m/%d/%Y %H:%M"),
         ADT_DATE = date(ADT_ARRIVAL_D),
         ADT_WEEKDAYS = wday(ADT_ARRIVAL_D, label = T),
         ADT_HOUR = hour(ADT_ARRIVAL_D),
         ADT_MONTH = factor(month(ADT_ARRIVAL_D),
                            levels = c('1', '2', '3', '4', '5', '6', '7','8', '9', '10', '11', '12'),
                            labels = c('January','February','March','April','May','June','July',
                                       'August','September','October','November','December'))
  ) %>%
  group_by(ADT_HOUR, ADT_DATE, ADT_WEEKDAYS, ADT_MONTH) %>%
  summarize(bp_general = sum(BEHAVIORAL_DASHB),
            n = n()) %>%
  ungroup()

bp <- inner_join(d, dc, by = c("ADT_DATE", "ADT_HOUR", "ADT_MONTH", "ADT_WEEKDAYS"))
saveRDS(bp, "bp.rds")
colnames(diagnose)
colnames(chiefComplaint)
diagnose$DX_NAME[1:5]
chiefComplaint$REASON_VISIT_NAME[1:5]

cc <- unique(chiefComplaint$REASON_VISIT_NAME)
diagnose$BEHAVIORAL_JASON[1:5]
str(diagnose)
head(diagnose)

