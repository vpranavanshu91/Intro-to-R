tfa <- read.csv("C:/Users/vpran/Desktop/Wharton_TFA/TFA.csv")

table(tfa$AP_Disposition_Step,tfa$UD_US_World_News_Selectivity)

tfa_2016 <- tfa[tfa$AP_Year=="2016",]
tfa_2017 <- tfa[tfa$AP_Year=="2017",]

school_info <- unique.data.frame(tfa[c(6,seq(16,25))])

Total_school_ls <- read.csv("C:/Users/vpran/Desktop/Wharton_TFA/Total_School_List.csv")

schools_not_recruited <- Total_school_ls[!(Total_school_ls$University.ID %in% school_info$AP_Undergrad_University_ID),]


schools_recruited_no_data <- school_info[!(school_info$AP_Undergrad_University_ID %in% Total_school_ls$University.ID),]
schools_recruited_no_data <- as.data.frame(schools_recruited_no_data[,1])


uniq_tfa_2017 <- as.data.frame(unique(tfa_2017$AP_Undergrad_University_ID))
uniq_tfa_2016 <- as.data.frame(unique(tfa_2016$AP_Undergrad_University_ID))
colnames(uniq_tfa_2016) <- c("U_ID")
colnames(uniq_tfa_2016) <- c("U_ID")

school_revisited <- as.data.frame(uniq_tfa_2016[uniq_tfa_2016$U_ID %in% uniq_tfa_2017$U_ID,])
new_school_visited <- as.data.frame(uniq_tfa_2017[!(uniq_tfa_2017$U_ID %in% uniq_tfa_2016$U_ID),])
school_not_revisited <- as.data.frame(uniq_tfa_2016[!(uniq_tfa_2016$U_ID %in% uniq_tfa_2017$U_ID),])

colnames(school_revisited) <- c("U_ID")
colnames(new_school_visited) <- c("U_ID")
colnames(school_not_revisited) <- c("U_ID")

#################TFA 2016###########################################################################

ftable(tfa_2016$AP_Disposition_Step,tfa_2016$AP_Adjusted_School_List)



