####### CODED BY ########

#Created: Yashaswini (6th Dec 2021)
# Edited: Yashaswini (9th Dec 2021)
# Edited: Aysu (11th Dec 2021)

####### SETTING PATHS FOR DIFFERENT USERS ############## (Created: Yashaswini)

Paths = c("/Users/yashaswinishekhawat/Documents/Applied-Micro-Dohmen", "/Users/aysuavci/Desktop/Applied_RM/Applied-Micro-Dohmen", "C:/Users/dell/Desktop/Applied-Micro-Dohmen", "C:/Users/Felix/Documents/RM_Dohmen/Applied-Micro-Dohmen","/Users/zixin/Desktop/Applied-Micro-Dohmen")
names(Paths) = c("yashaswinishekhawat","aysuavci", "dell", "Felix","zixin")
setwd(Paths[Sys.info()[7]])


###### READING IN THE DATA ########### (Created: Yashaswini)


## Loading all relevant libraries

library(dplyr)

input <- "Outputs/main.csv"
output <- "Outputs/main.csv"

dataSOEP <- read.csv(file=input)


####### CREATED VARIABLES ######## ()

#Note: create vars with different variable names and clear titles, don't alter original vars

#Creating Age Variable

#harmonize birth year: created var birth_year
dataSOEP <- dataSOEP %>%
  group_by(pid) %>% 
  mutate(birth_year= max(ple0010_h))

dataSOEP <- subset(dataSOEP, dataSOEP$birth_year >0 & dataSOEP$syear >0)



###### CHECKING CORRELATION BETWEEN GIVE_AWAY 2010 AND 2017 ###### (Created: Yashaswini)

# Checking corr
data_2010 <- subset(dataSOEP, dataSOEP$syear==2010 & is.na(dataSOEP$giveaway)==FALSE, select= c("pid", "giveaway", "pglabnet"))
data_2017 <- subset(dataSOEP, dataSOEP$syear==2017 & is.na(dataSOEP$giveaway)==FALSE, select= c("pid", "giveaway", "pglabnet"))
corr_data <- inner_join(data_2010, data_2017, by= "pid", suffix= c("_2010", "_2017"))
cor(corr_data$giveaway_2010, corr_data$giveaway_2017)

# Checking if change in income explains change in give away value
corr_data$diffgive <- corr_data$giveaway_2017-corr_data$giveaway_2010
corr_data$diffincome <- corr_data$pglabnet_2017- corr_data$pglabnet_2010
corr_data <- na.omit(corr_data)
diff <- lm(diffgive~diffincome, data=corr_data)
summary(diff)

# Checking corr between residuals, controlling for income
x1 <- lm(giveaway_2010 ~ pglabnet_2010, data=corr_data)
x2 <- lm(giveaway_2017 ~ pglabnet_2017, data=corr_data)
cor(resid(x1), resid(x2))

# Creating NEW VARS: giveaway_2010, giveaway_2017, giveaway_avg
data_2010A <- subset(dataSOEP, dataSOEP$syear==2010 & is.na(dataSOEP$giveaway)==FALSE, select= c("pid", "giveaway"))
data_2017A <- subset(dataSOEP, dataSOEP$syear==2017 & is.na(dataSOEP$giveaway)==FALSE, select= c("pid", "giveaway"))
corrdata1 <- full_join(data_2010A, data_2017A, by= "pid", suffix= c("_2010", "_2017"))
dataSOEP <- left_join(dataSOEP, corrdata1, by= "pid")
dataSOEP$giveaway_avg <- rowMeans(dataSOEP[,c("giveaway_2010", "giveaway_2017")], na.rm=TRUE)
dataSOEP$diff_giveaway <- dataSOEP$giveaway_2017- dataSOEP$giveaway_2010

###### Life Goal Quesitons 2008 AND 2016 ###### (Created: Aysu)

# Creating NEW VARS: giveaway_2010, giveaway_2017, giveaway_avg
data_2008A <- subset(dataSOEP, dataSOEP$syear==2008 & is.na(dataSOEP$lifegoal_help)==FALSE & is.na(dataSOEP$lifegoal_so_po)==FALSE, select= c("pid", "lifegoal_help", "lifegoal_so_po"))
data_2016A <- subset(dataSOEP, dataSOEP$syear==2016 & is.na(dataSOEP$lifegoal_help)==FALSE & is.na(dataSOEP$lifegoal_so_po)==FALSE, select= c("pid", "lifegoal_help", "lifegoal_so_po"))
corrdata2 <- full_join(data_2008A, data_2016A, by= "pid", suffix= c("_2008", "_2016"))
dataSOEP <- left_join(dataSOEP, corrdata2, by= "pid")


###### Religion from 2011 ###### (Created: Aysu)
corrdata3 <- dataSOEP %>%
  select(religion, pid, syear) %>% 
  filter(syear==2011) %>%
  mutate(religion_2011 = religion)%>%
  select(religion_2011, pid)

###### Political Attitude from 2009 ###### (Created: Aysu)
corrdata4 <- dataSOEP %>%
  select(political_attitude, pid, syear) %>% 
  filter(syear==2009) %>%
  mutate(political_attitude_2009 = political_attitude)%>%
  select(political_attitude_2009, pid)

  
dataSOEP <- left_join(dataSOEP, corrdata3, by= "pid")
dataSOEP <- left_join(dataSOEP, corrdata4, by= "pid")

###### Religion from 2015 ###### (Created: Zixin)
corrdata5 <- dataSOEP %>%
  select(religion, pid, syear) %>% 
  filter(syear==2015) %>%
  mutate(religion_2015 = religion)%>%
  select(religion_2015, pid)

###### Political Attitude from 2014 ###### (Created: Zixin)
corrdata6 <- dataSOEP %>%
  select(political_attitude, pid, syear) %>% 
  filter(syear==2014) %>%
  mutate(political_attitude_2014 = political_attitude)%>%
  select(political_attitude_2014, pid)

dataSOEP <- left_join(dataSOEP, corrdata5, by= "pid")
dataSOEP <- left_join(dataSOEP, corrdata6, by= "pid")

###Creating volunteer work from 2008
corrdata7 <- dataSOEP %>%
        select(volunteer_work, pid, syear) %>% 
        filter(syear==2008) %>%
        mutate(volunteer_work_2008 = volunteer_work)%>%
        select(volunteer_work_2008, pid)
dataSOEP <- left_join(dataSOEP, corrdata7, by= "pid")

#Creating Gender Dummy and reverse coding life_goal question
dataSOEP <- dataSOEP %>% 
  mutate(female = ifelse(sex == 2, 1, 0),
         lifegoal_help_r = case_when(lifegoal_help==1 ~ 4,
                                     lifegoal_help==2 ~ 3,
                                     lifegoal_help==3 ~ 2,
                                     lifegoal_help==4 ~ 1),
         lifegoal_so_po_r = case_when(lifegoal_so_po==1 ~ 4,
                                      lifegoal_so_po==2 ~ 3,
                                      lifegoal_so_po==3 ~ 2,
                                      lifegoal_so_po==4 ~ 1),
         lifegoal_help_2008_r = case_when(lifegoal_help_2008==1 ~ 4,
                                          lifegoal_help_2008==2 ~ 3,
                                          lifegoal_help_2008==3 ~ 2,
                                          lifegoal_help_2008==4 ~ 1),
         lifegoal_so_po_2008_r = case_when(lifegoal_so_po_2008==1 ~ 4,
                                           lifegoal_so_po_2008==2 ~ 3,
                                           lifegoal_so_po_2008==3 ~ 2,
                                           lifegoal_so_po_2008==4 ~ 1),
         lifegoal_help_2016_r = case_when(lifegoal_help_2016==1 ~ 4,
                                          lifegoal_help_2016==2 ~ 3,
                                          lifegoal_help_2016==3 ~ 2,
                                          lifegoal_help_2016==4 ~ 1),
         lifegoal_so_po_2016_r = case_when(lifegoal_so_po_2016==1 ~ 4,
                                           lifegoal_so_po_2016==2 ~ 3,
                                           lifegoal_so_po_2016==3 ~ 2,
                                           lifegoal_so_po_2016==4 ~ 1),
         #"Daily" coded in "At least once a week"
         volunteer_work_r = case_when((volunteer_work==1|volunteer_work==2) ~ 4,
                                      volunteer_work==3 ~ 3,
                                      volunteer_work==4 ~ 2,
                                      volunteer_work==5 ~ 1))

#divide the giveaway and income variable by 1000 for ease of interpretation
dataSOEP$giveaway_divided<-dataSOEP$giveaway/1000
dataSOEP$giveaway_avg_divided<- dataSOEP$giveaway_avg/1000
dataSOEP$giveaway_2010_divided<- dataSOEP$giveaway_2010/1000
dataSOEP$giveaway_2017_divided<- dataSOEP$giveaway_2017/1000
dataSOEP$diff_giveaway_divided<- dataSOEP$diff_giveaway/1000
dataSOEP$netincome_divided <- dataSOEP$pglabnet/1000

#Calculating household income, deleted after adding generated hh income
#dataSOEP_groupby_hh <- dataSOEP %>%
  #group_by(hid) %>%
  #mutate(hh_netincome= sum(pglabnet, na.rm=TRUE)) %>%
  #select(hid, hh_netincome, syear, pid)

#Adding the variable to the original dataset
#dataSOEP <- dataSOEP %>%
  #left_join(dataSOEP_groupby_hh, by= c("hid", "syear", "pid"))

dataSOEP$hh_netincome_divided <- dataSOEP$hh_netincome/1000

######## Change in altruistic capital between 2010 and 2017 ###### (Created: Yashaswini)

### If money donated, volunteer work etc over the period 2010-2017 then takes value 1, else 0

data_cap1 <- subset(dataSOEP, dataSOEP$syear==2015, select= c("pid", "money_subscribe"))
data_cap1 <- data_cap1 %>%
             mutate(money_capital2015= case_when(money_subscribe==1L ~ 1L, 
                                                money_subscribe==2L ~ 0L,
                                                TRUE ~ as.integer(NA))) %>%
            select(!c(money_subscribe))

data_cap2 <- subset(dataSOEP, dataSOEP$syear==2011, select= c("pid", "volunteer_work_r"))
data_cap2 <- data_cap2 %>%
              mutate(volunteer_capital2011= case_when(volunteer_work_r==1L ~ 0L, 
                                                    volunteer_work_r==2L ~ 1L,
                                                    volunteer_work_r==3L ~ 1L,
                                                    volunteer_work_r==4L ~ 1L,
                                                    TRUE ~ as.integer(NA))) %>%
              select(!c(volunteer_work_r))

data_cap3 <- subset(dataSOEP, dataSOEP$syear==2013, select= c("pid", "volunteer_work_r"))
data_cap3 <- data_cap3 %>%
              mutate(volunteer_capital2013= case_when(volunteer_work_r==1L ~ 0L, 
                                                    volunteer_work_r==2L ~ 1L,
                                                    volunteer_work_r==3L ~ 1L,
                                                    volunteer_work_r==4L ~ 1L,
                                                    TRUE ~ as.integer(NA))) %>%
              select(!c(volunteer_work_r))

data_cap4 <- subset(dataSOEP, dataSOEP$syear==2015, select= c("pid", "volunteer_work_r"))
data_cap4 <- data_cap4 %>%
             mutate(volunteer_capital2015= case_when(volunteer_work_r==1L ~ 0L, 
                                                    volunteer_work_r==2L ~ 1L,
                                                    volunteer_work_r==3L ~ 1L,
                                                    volunteer_work_r==4L ~ 1L,
                                                    TRUE ~ as.integer(NA))) %>%
            select(!c(volunteer_work_r))

data_cap5 <- subset(dataSOEP, dataSOEP$syear==2015, select= c("pid", "blood_donation_fiveyears"))
data_cap5 <- data_cap5 %>%
             mutate(blood_capital2015= case_when(blood_donation_fiveyears==1L ~ 1L,
                                               blood_donation_fiveyears==2L ~ 0L,
                                               TRUE ~ as.integer(NA))) %>%
             select(!c(blood_donation_fiveyears))

dataSOEP <- left_join(dataSOEP, data_cap1, by="pid")
dataSOEP <- left_join(dataSOEP, data_cap2, by="pid")
dataSOEP <- left_join(dataSOEP, data_cap3, by="pid")
dataSOEP <- left_join(dataSOEP, data_cap4, by="pid")
dataSOEP <- left_join(dataSOEP, data_cap5, by="pid")

## Adding a common volunteer capital variable (Created: Yashaswini)

dataSOEP <- dataSOEP %>%
  mutate(volunteer_capital= case_when(volunteer_capital2011==1L ~ 1L, 
                                      volunteer_capital2011==0L ~ 0L,
                                      volunteer_capital2013==1L ~ 1L, 
                                      volunteer_capital2013==0L ~ 0L,
                                      volunteer_capital2015==1L ~ 1L, 
                                      volunteer_capital2015==0L ~ 0L,
                                          TRUE ~ as.integer(NA)))


#Standardize the measures
dataSOEP$s_giveaway_divided <- (dataSOEP$giveaway_divided - mean(dataSOEP$giveaway_divided, na.rm=TRUE)) / sd(dataSOEP$giveaway_divided, na.rm = TRUE)
dataSOEP$s_giveaway <- (dataSOEP$giveaway - mean(dataSOEP$giveaway, na.rm = TRUE)) / sd(dataSOEP$giveaway, na.rm = TRUE)
dataSOEP$s_giveaway_avg_divided <- (dataSOEP$giveaway_avg_divided - mean(dataSOEP$giveaway_avg_divided, na.rm = TRUE)) / sd(dataSOEP$giveaway_avg_divided, na.rm = TRUE)
dataSOEP$s_giveaway_2010_divided <- (dataSOEP$giveaway_2010_divided - mean(dataSOEP$giveaway_2010_divided, na.rm = TRUE)) / sd(dataSOEP$giveaway_2010_divided, na.rm = TRUE)
dataSOEP$s_giveaway_2017_divided <- (dataSOEP$giveaway_2017_divided - mean(dataSOEP$giveaway_2017_divided, na.rm = TRUE)) / sd(dataSOEP$giveaway_2017_divided, na.rm = TRUE)
dataSOEP$s_lifegoal_help_r <- (dataSOEP$lifegoal_help_r - mean(dataSOEP$lifegoal_help_r, na.rm = TRUE)) / sd(dataSOEP$lifegoal_help_r, na.rm = TRUE)
dataSOEP$s_lifegoal_so_po_r <- (dataSOEP$lifegoal_so_po_r - mean(dataSOEP$lifegoal_so_po_r, na.rm = TRUE)) / sd(dataSOEP$lifegoal_so_po_r, na.rm = TRUE)
dataSOEP$s_lifegoal_help_2008_r <- (dataSOEP$lifegoal_help_2008_r - mean(dataSOEP$lifegoal_help_2008_r, na.rm = TRUE)) / sd(dataSOEP$lifegoal_help_2008_r, na.rm = TRUE)
dataSOEP$s_lifegoal_so_po_2008_r <- (dataSOEP$lifegoal_so_po_2008_r - mean(dataSOEP$lifegoal_so_po_2008_r, na.rm = TRUE)) / sd(dataSOEP$lifegoal_so_po_2008_r, na.rm = TRUE)
dataSOEP$s_lifegoal_help_2016_r <- (dataSOEP$lifegoal_help_2016_r - mean(dataSOEP$lifegoal_help_2016_r, na.rm = TRUE)) / sd(dataSOEP$lifegoal_help_2016_r, na.rm = TRUE)
dataSOEP$s_lifegoal_so_po_2016_r <- (dataSOEP$lifegoal_so_po_2016_r - mean(dataSOEP$lifegoal_so_po_2016_r, na.rm = TRUE)) / sd(dataSOEP$lifegoal_so_po_2016_r, na.rm = TRUE)

# Adding the diff vars for lifegoals (Created: Yashaswini)

dataSOEP$diff_lifegoal_help <- dataSOEP$s_lifegoal_help_2016_r - dataSOEP$s_lifegoal_help_2008_r
dataSOEP$diff_lifegoal_sopo <- dataSOEP$s_lifegoal_so_po_2016_r - dataSOEP$s_lifegoal_so_po_2008_r

write.csv(dataSOEP, output)
