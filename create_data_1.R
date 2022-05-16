####### CODED BY ########

#Created: Yashaswini Shekhawat
#Edited: Aysu (date: 29th nov 2021); Yashaswini (date: 26th nov 2021)
 

#######  FOR SETTING PATHS FOR DIFFERENT USERS ##############

## Setting paths for different users: just add your working directorypath to it and your system name to the path
## For eg: Paths = c("C://user/Yashaswini", "C://user/Zixin", "C://Some/other/Felix", "C://user/Aysu")
## names(Paths) = c("yashaswini", "zixin", "felix", "aysu")
## Specify the names as they appear in the system info (sys.info): then for current user, R will set the working directory

Paths = c("/Users/yashaswinishekhawat/Documents/Applied-Micro-Dohmen", "/Users/aysuavci/Desktop/Applied_RM/Applied-Micro-Dohmen", "C:/Users/dell/Desktop/Applied-Micro-Dohmen", "C:/Users/Felix/Documents/RM_Dohmen/Applied-Micro-Dohmen", "/Users/zixin/Desktop/Applied-Micro-Dohmen")
names(Paths) = c("yashaswinishekhawat","aysuavci", "dell", "Felix","zixin")
setwd(Paths[Sys.info()[7]])

###### READING IN THE DATA AND SELECTING SPECIFIED VARIABLES ###########


## Loading all relevant libraries
## for package installs: install.packages("name")
library(haven)
library(dplyr)


#Just in case anyone needs to change something in the way the input/output is specified for their system
input_pl <- "Inputs/pl.dta"
input_pgen <- "Inputs/pgen.dta"
input_bio <- "Inputs/biol.dta"
input_ppath <- "Inputs/ppathl.dta"
input_hgen <- "Inputs/hgen.dta"
output <- "Outputs/main.csv"

pl <- read_dta(input_pl, col_select = c(plh0129, plh0130, plh0131_v1, plh0131_v2, plh0132, plh0133, plh0134, 
                                          plh0135, plh0136, pli0095_h, pli0096_h, plj0438, plj0440, plj0442,
                                          plj0439, plj0441, plj0443, plh0212, plh0213, plh0214, plh0215, 
                                          plh0216, plh0217, plh0218, plh0219, plh0220, plh0221, plh0222, 
                                          plh0223, plh0224, plh0225, plh0226, plh0255,plh0104, plh0105, 
                                          plh0106, plh0107, plh0108, plh0109, plh0110, plh0111,
                                          plh0112, pid, syear, hid, plh0258_h, plh0011_h, plh0012_h, plh0013_h, 
                                          plh0004,ple0010_h, ple0008, plh0111, plh0090, plh0004))

# names(data) to view variable names, useful for data with not too many columns

## I added nationality, marital status, income, job classifications and years of education
## You can take a look at the document pgen and discuss adding more:
## https://www.diw.de/documents/publikationen/73/diw_01.c.571093.de/diw_ssp0307.pdf
pgen <- read_dta(input_pgen, col_select = c(pid, syear, pgnation, pgfamstd, pglabgro, pglabnet, pgsndjob, pgstib, pgemplst, 
                                            pgisco88, pgisco08, pgkldb2010, pgtatzeit, pgvebzeit, pguebstd, 
                                            pgoeffd, pgnace, pgnace2, pgexpue, pgbilzeit))

## I only added child info, religion and year born, more can be added:
## https://www.diw.de/documents/publikationen/73/diw_01.c.821956.de/diw_ssp0957.pdf
bio <- read_dta(input_bio, col_select = c(pid, syear, lb0286_h, lb0285, lb0880, lb0881, lb0011_h))

## Adding the sex variable from the ppath data
ppath <- read_dta(input_ppath, col_select = c(pid, syear, sex))

## Adding hh net income
hgen <- read_dta(input_hgen, col_select = c(hid, syear, hghinc))


########## DATA CLEANING AND RESHAPING ##########

#variables <- c("plh0129", "plh0130", "plh0131_v1", "plh0131_v2", "plh0132", "plh0133", "plh0134", "plh0135", "plh0136", "pli0095_h", "pli0096_h", "plj0438", "plj0440", "plj0442", "plj0439", "plj0441", "plj0443", "plh0212", "plh0213", "plh0214", "plh0215", "plh0216", "plh0217", "plh0218", "plh0219", "plh0220", "plh0221", "plh0222", "plh0223", "plh0224", "plh0225", "plh0226", "plh0255","plh0104", "plh0105", "plh0106", "plh0107", "plh0108", "plh0109", "plh0110", "plh0111","plh0112", "pid", "syear", "hid", "plh0258_h", "plh0011_h", "plh0012_h", "plh0013_h", "plh0004", "ple0010_h")

#data1 <- subset(data, select= variables)
pl_clean <- pl %>%
  mutate(giveaway = ifelse(plh0135<0, NA, plh0135),# data cleaning, make the negative values NA
         health = ifelse(ple0008<0, NA, ple0008),
         lifegoal_help = ifelse(plh0105<0, NA, plh0105),
         lifegoal_so_po = ifelse(plh0111<0, NA, plh0111),
         lifegoal_friends = ifelse(plh0090<0, NA, plh0090),
         political_attitude = ifelse(plh0004<0, NA, plh0004),
         religion = ifelse(plh0258_h<0, NA, plh0258_h),
         refu_money_donation = ifelse(plj0438<0, NA, plj0438),
         refu_work_with = ifelse(plj0440<0, NA, plj0440),
         refu_demonstrations = ifelse(plj0442<0, NA, plj0442),
         refu_money_donation_p = ifelse(plj0439<0, NA, plj0439),
         refu_work_with_p = ifelse(plj0441<0, NA, plj0441),
         refu_demonstrations_p = ifelse(plj0443<0, NA, plj0443),#we can also write a function but then we cannot rename them
         help_friends = ifelse(pli0095_h<0, NA, pli0095_h),
         volunteer_work = ifelse(pli0096_h<0, NA, pli0096_h),
         money_subscribe = ifelse(plh0129<0, NA, plh0129),
         money_subscribe_amount = ifelse(plh0130<0, NA, plh0130),
         blood_donation_fiveyears = ifelse(plh0131_v2<0, NA, plh0131_v2),
         blood_donation_tenyears = ifelse(plh0131_v1<0, NA, plh0131_v1),
         blood_donation_lastyear = ifelse(plh0132<0, NA, plh0132),
         medicalreason_no_donation = ifelse(plh0133<0, NA, plh0133)
         ) %>%
  select(!c(plh0129, plh0130, plh0131_v1, plh0131_v2, plh0132, plh0133,
            plh0135, pli0095_h, pli0096_h, plj0438, plj0440, plj0442, plj0439, 
            plj0441, plj0443, plh0105, ple0008, plh0090, plh0111, plh0004))

# Rename the pgen and hgen
names(pgen)[names(pgen) == "pgemplst"] <- "emp_status" #full time, part time, etc.

names(hgen)[names(hgen) == "hghinc"] <- "hh_netincome"

## We can make all the negative values NA for these datasets I guess
## I didn't want to do it for the first data set since negative values might become useful
## If they won't we can change the code in last code cleaning

pgen_clean <- pgen
pgen_clean[pgen_clean<0] <- NA

bio_clean <- bio
bio_clean[bio_clean<0] <- NA

ppath_clean <- ppath
ppath_clean[ppath_clean<0] <- NA

## Keep the last year of the generated info, but maybe we might want to restrict our analysis to one year
## Let's say if we were to look at the answers given to a question in 2015
## we might want to only take employment info in 2015
#pgen_clean <- pgen_clean %>%
#arrange(pid,syear) %>%
#filter(duplicated(pid, fromLast=T)==F) 

#bio_clean <- bio_clean %>%
#arrange(pid,syear) %>%
#filter(duplicated(pid, fromLast=T)==F) 

#ppath_clean <- ppath_clean %>%
#filter(duplicated(pid)==F) 
  
########## DATA MERGING ##########

## Merging the two data sets by personal id (pid)

dataSOEP <- pl_clean %>%
  left_join(pgen_clean, by = c("pid", "syear")) #add syear

dataSOEP <- dataSOEP %>%
  left_join(bio_clean, by = c("pid", "syear")) 

dataSOEP <- dataSOEP %>%
  left_join(ppath_clean, by = c("pid", "syear")) 

dataSOEP <- dataSOEP %>%
  left_join(hgen, by = c("hid", "syear")) 

write.csv(dataSOEP, output)
#write.csv(data1, output)





