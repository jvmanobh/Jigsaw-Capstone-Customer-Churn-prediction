#Loading Libraries
library(dplyr)
library(ggplot2)
options(scipen = 999)
library(pROC)
library(caret)
library(fastDummies)
library(MASS)
library(leaps)
library(ROCR)



setwd("C:\\Users\\346487\\Desktop\\Capstone\\")

# Reading Dataset
tel <- read.csv("C:\\Users\\346487\\Desktop\\Capstone\\Telecom_Sampled.csv")

#Checkind Structure of data
str(tel)
#81 variables with 26518 records

#checking Dependent Variable Churn
#Instances of customer leaving the telecom within 31-60 days after the data observation date

#Checking for Missing values
sum(is.na(tel$churn))
#No Missing values

#Checking the percentage ditribution
table(tel$churn)/nrow(tel)
#Almost 24% of people are leaving the telecom within 31-60 days

#There are 58 records with null values for all the following 12 columns
#columns ovrmou_Mean, ovrrev_Mean, datovr_Mean, datovr_Range, mou_Mean, mou_Range, rev_Mean, rev_Range,
#totmrc_Mean, roam_Mean, da_Mean, da_Range
#Removing these rows
sum(is.na(tel$ovrmou_Mean) & is.na(tel$ovrrev_Mean) & is.na(tel$datovr_Mean) & is.na(tel$datovr_Range) &
    is.na(tel$mou_Mean) & is.na(tel$mou_Range) & is.na(tel$rev_Mean) & is.na(tel$rev_Range) &
    is.na(tel$totmrc_Mean) & is.na(tel$roam_Mean) & is.na(tel$da_Mean) & is.na(tel$da_Range))

tel[!is.na(tel$mou_Mean),] -> tel
str(tel)
#81 variables with 26460 records


#creating data quality report
variables <- names(tel)
dqr <- as.data.frame(variables)
#Getting Data type
dqr$Datatype <- sapply(tel, class)
#Getting number of records
dqr$NoOfRecords <- sapply(tel, length)
#Getting count of unique values in each column
dqr$UniqueRecords <- sapply(tel, function(x) length(unique(x)))
#Getting count of Non-Missing vlaues
dqr$DataAvailable <- sapply(tel, function(x) sum(!is.na(x)))
#Getting Available Values Percent
dqr$AvailablePercent <- round(100*dqr$DataAvailable/dqr$NoOfRecords, 2)
#Getting count of Missing vlaues
dqr$Missing <- sapply(tel, function(x) sum(is.na(x)))
#Getting Missing Values Percent
dqr$MissingPercent <- round(100*dqr$Missing/dqr$NoOfRecords, 2)
#Getting Minimum Value
dqr$Minimum <- sapply(tel, function(x) if (class(x)=='factor') NA else round(min(x, na.rm = TRUE),2))
#Getting Maxmum Value
dqr$Maximum <- sapply(tel, function(x) if (class(x)=='factor') NA else round(max(x, na.rm = TRUE),2))
#Getting Mean Value
dqr$Mean <- sapply(tel, function(x) if (class(x)=='factor') NA else round(mean(x, na.rm = TRUE),2))
#Getting 5th Percentile
dqr['5th Percentile'] <- sapply(tel, function(x) if (class(x)=='factor') NA else round(quantile(x, 0.05, na.rm = TRUE),2))
#Getting 10th Percentile
dqr['10th Percentile'] <- sapply(tel, function(x) if (class(x)=='factor') NA else round(quantile(x, 0.1, na.rm = TRUE),2))
#Getting 25th Percentile
dqr['25th Percentile'] <- sapply(tel, function(x) if (class(x)=='factor') NA else round(quantile(x, 0.25, na.rm = TRUE),2))
#Getting 50th Percentile
dqr['50th Percentile'] <- sapply(tel, function(x) if (class(x)=='factor') NA else round(quantile(x, 0.5, na.rm = TRUE),2))
#Getting 75th Percentile
dqr['75th Percentile'] <- sapply(tel, function(x) if (class(x)=='factor') NA else round(quantile(x, 0.75, na.rm = TRUE),2))
#Getting 90th Percentile
dqr['90th Percentile'] <- sapply(tel, function(x) if (class(x)=='factor') NA else round(quantile(x, 0.9, na.rm = TRUE),2))
#Getting 95th Percentile
dqr['95th Percentile'] <- sapply(tel, function(x) if (class(x)=='factor') NA else round(quantile(x, 0.95, na.rm = TRUE),2))

write.csv(dqr, file = "data_quality_report.csv", row.names = FALSE)



#Checking Variables with missing values more than 10%
sort(colSums(is.na(tel))/nrow(tel), decreasing = TRUE)
#14 Variables have missing values more than 10%
#solflag, retdays, wrkwoman, div_type, occu1, proptype, cartype, children, mailordr, mailresp, numbcars,
#dwllsize, dwlltype, income

#From the Data documentation,
#solflag - missing values indicate deafault preference, Mostly the telecoms will setup this flag as 'N' by default
#retdays - Missing values for this variable means there have been no retention calls made by the customer in the past
#div_type - Represents additional services, May be the missing values represent No Additional services

#For all the other 11 variables, missing values doesn't mean anything, It represents information is not available
#Hence removing the below 11 varaibles
#wrkwoman, occu1, proptype, cartype, children, mailordr, mailresp, numbcars, dwllsize, dwlltype, income

tel %>% dplyr::select(-wrkwoman,-occu1,-proptype,-cartype,-children,-mailordr,-mailresp,-numbcars,-dwllsize,-dwlltype,
               -income) -> tel
str(tel)
#70 variables with 26460 records


#Checking continous variables
#mou_Mean - Mean number of monthly minutes of use, No missing values
quantile(tel$mou_Mean, c(0.99, 0.995))
names(tel)

num_col = list(c("mou_Mean", "totmrc_Mean", "rev_Range", "mou_Range", "change_mou", "drop_blk_Mean",
                 "drop_vce_Range", "owylis_vce_Range", "mou_opkv_Range", "months", "totcalls", "eqpdays",
                 "custcare_Mean", "callwait_Mean", "iwylis_vce_Mean", "callwait_Range", "ccrndmou_Range",
                 "adjqty", "ovrrev_Mean", "rev_Mean", "ovrmou_Mean", "comp_vce_Mean", "plcd_vce_Mean",
                 "avg3mou", "avgmou", "avg3qty", "avgqty", "avg6mou", "avg6qty", "drop_dat_Mean", "drop_vce_Mean",
                 "adjmou", "totrev", "adjrev", "avgrev", "comp_dat_Mean", "plcd_dat_Mean",
                 "roam_Mean", "recv_sms_Mean", "blck_dat_Mean", "mou_pead_Mean", "opk_dat_Mean",
                 "hnd_price", "datovr_Mean", "datovr_Range", "da_Mean", "da_Range"))
                 
cat_col = list(c("retdays", "asl_flag", "prizm_social_one", "area", "refurb_new", "hnd_webcap", "crclscod",
                 "ethnic", "models", "actvsubs", "marital", "uniqsubs", "forgntvl", "mtrcycle", "truck",
                 "solflag", "car_buy", "csa", "div_type"))

#Treating unacceptable and missing values in data. Also checking category variables
#Variable totmrc_Mean
sum(tel$totmrc_Mean<0)
#4 values in totmrc_Mean is less than zero
head(sort(cor(tel[, num_col[[1]]])[, "totmrc_Mean"], decreasing = TRUE), 5)
head(sort(cor(tel[, num_col[[1]]])[, "totmrc_Mean"]), 5)
#Strong correlation with avgrev, Hence imputing with linear model
ols <- lm(totmrc_Mean~avgrev, data=tel[tel$totmrc_Mean>=0,])
tel[tel$totmrc_Mean<0, 'totmrc_Mean'] <- predict(ols,newdata=tel[tel$totmrc_Mean<0,])

#Variable change_mou
sum(is.na(tel$change_mou))
#103 missing values
head(sort(cor(tel[!is.na(tel$change_mou), num_col[[1]]])[, "change_mou"], decreasing = TRUE), 5)
head(sort(cor(tel[!is.na(tel$change_mou), num_col[[1]]])[, "change_mou"]), 5)
#No strong correlation
plot(density(tel[!is.na(tel$change_mou), "change_mou"]))
#Normal density plot, imputing with Median
tel[is.na(tel$change_mou), 'change_mou'] <- median(tel$change_mou, na.rm = TRUE)

#Variable eqpdays
sum(tel$eqpdays<0)
head(sort(cor(tel[, num_col[[1]]])[, "eqpdays"], decreasing = TRUE), 5)
head(sort(cor(tel[, num_col[[1]]])[, "eqpdays"]), 5)
#No strong correlation
plot(density(tel[!is.na(tel$eqpdays), "eqpdays"]))
#Right skewness, imputing with Median
tel[tel$eqpdays<0, 'eqpdays'] <- median(tel$eqpdays)

#variable rev_Mean
sum(tel$rev_Mean<0)
#1 value with less than zero
head(sort(cor(tel[, num_col[[1]]])[, "rev_Mean"], decreasing = TRUE), 5)
head(sort(cor(tel[, num_col[[1]]])[, "rev_Mean"]), 5)
#Strong correlation with ovrrev_Mean, avgrev, Hence imputing with linear model
ols <- lm(rev_Mean~ovrrev_Mean+avgrev, data=tel[tel$rev_Mean>=0,])
tel[tel$rev_Mean<0, 'rev_Mean'] <- predict(ols,newdata=tel[tel$rev_Mean<0,])

#variable avg6mou
sum(is.na(tel$avg6mou))
#808 missing values
head(sort(cor(tel[!is.na(tel$avg6mou), num_col[[1]]])[, "avg6mou"], decreasing = TRUE), 5)
head(sort(cor(tel[!is.na(tel$avg6mou), num_col[[1]]])[, "avg6mou"]), 5)
#Strong correlation with variables avg3mou, avgmou, Hence imputing with linear model
ols <- lm(avg6mou~avg3mou+avgmou, data=tel[!is.na(tel$avg6mou),])
tel[is.na(tel$avg6mou), 'avg6mou'] <- predict(ols,newdata=tel[is.na(tel$avg6mou),])

#variable avg6qty
sum(is.na(tel$avg6qty))
#808 missing values
head(sort(cor(tel[!is.na(tel$avg6qty), num_col[[1]]])[, "avg6qty"], decreasing = TRUE), 5)
head(sort(cor(tel[!is.na(tel$avg6qty), num_col[[1]]])[, "avg6qty"]), 5)
#Strong correlation with variables avg3qty, avgqty, Hence imputing with linear model
ols <- lm(avg6qty~avg3qty+avgqty, data=tel[!is.na(tel$avg6qty),])
tel[is.na(tel$avg6qty), 'avg6qty'] <- predict(ols,newdata=tel[is.na(tel$avg6qty),])


#variable crclscod
tel$crclscod1<- substr(tel$crclscod, 1, 1)
tel %>% group_by(crclscod1) %>% summarise(mean=mean(churn),count=n()) %>% arrange(mean)
#Grouping by churn rate
#D, E, V, H, Y as D_E_H_V_Y,
tel[tel$crclscod1 %in% c('D','E','V','H','Y'), 'crclscod1'] <- 'D_E_H_V_Y'
#U, W, I, C, Z, J, M as C_I_J_M_U_W_Z
tel[tel$crclscod1 %in% c('U','W','I','C','Z','J','M'), 'crclscod1'] <- 'C_I_J_M_U_W_Z'
#G, A as A_G
tel[tel$crclscod1 %in% c('G','A'), 'crclscod1'] <- 'A_G'
#T, O, K, B AS B_K_O_T
tel[tel$crclscod1 %in% c('T','O','K','B'), 'crclscod1'] <- 'B_K_O_T'
tel$crclscod <- NULL
dum_cat <- 'crclscod1'


#variable prizm_social_one - Social group. Based on degree of population density of area
sum(is.na(tel$prizm_social_one))
#1878 missing values
tel %>% group_by(prizm_social_one) %>% summarise(mean=mean(churn), count=n())
#Missing value group's churn rate is close to category 'R', Hence imputing 'R' for missing values
tel[is.na(tel$prizm_social_one), 'prizm_social_one'] <- "R"
#Also churn rates of groups C, S, U, Hence changing them as 'C_S_U'
tel$prizm_social_one <- plyr::revalue(tel$prizm_social_one, c("C"="C_S_U", "S"="C_S_U", "U"="C_S_U"))
dum_cat <- c(dum_cat, 'prizm_social_one')


#variable area
sum(is.na(tel$area))
#5 missing values
tel %>% group_by(area) %>% summarise(mean=mean(churn), count=n()) %>% arrange(mean)
#Imputing with mode 'NEW YORK CITY AREA'
tel[is.na(tel$area), 'area'] <- "NEW YORK CITY AREA"
#Grouping based on churn rate
#Grouping CENTRAL/SOUTH TEXAS, MIDWEST, OHIO AS TEXAS_MIDWEST_OHIO
tel$area <- plyr::revalue(tel$area, c("CENTRAL/SOUTH TEXAS AREA"="TEXAS_MIDWEST_OHIO", "MIDWEST AREA"="TEXAS_MIDWEST_OHIO",
                                      "OHIO AREA"="TEXAS_MIDWEST_OHIO"))
#Grouping GREAT LAKES, HOUSTON, DC/MARYLAND/VIRGINIA, LOS ANGELES AS GRTLK_HOUST_DCMARYVIRG_LOSANG
tel$area <- plyr::revalue(tel$area, c("GREAT LAKES AREA"="GRTLK_HOUST_DCMARYVIRG_LOSANG", "HOUSTON AREA"="GRTLK_HOUST_DCMARYVIRG_LOSANG",
                                      "DC/MARYLAND/VIRGINIA AREA"="GRTLK_HOUST_DCMARYVIRG_LOSANG", "LOS ANGELES AREA"="GRTLK_HOUST_DCMARYVIRG_LOSANG"))
#Grouping DALLAS, SOUTHWEST, CHICAGO AS DALLAS_SOUTHWEST_CHICAGO
tel$area <- plyr::revalue(tel$area, c("DALLAS AREA"="DALLAS_SOUTHWEST_CHICAGO", "SOUTHWEST AREA"="DALLAS_SOUTHWEST_CHICAGO",
                                      "CHICAGO AREA"="DALLAS_SOUTHWEST_CHICAGO"))
#Grouping SOUTH FLORIDA, PHILADELPHIA, NORTH FLORIDA AS FLORIDA_PHILY
tel$area <- plyr::revalue(tel$area, c("SOUTH FLORIDA AREA"="FLORIDA_PHILY", "PHILADELPHIA AREA"="FLORIDA_PHILY",
                                      "NORTH FLORIDA AREA"="FLORIDA_PHILY", "NEW YORK CITY AREA"="NEWYORK",
                                      "NORTHWEST/ROCKY MOUNTAIN AREA"="NORTHWEST_ROCKY_MOUNTAIN",
                                      "CALIFORNIA NORTH AREA"="CALIFORNIA_NORTH", "NEW ENGLAND AREA"="NEWENGLAND"))
dum_cat <- c(dum_cat, 'area')


#variable hnd_webcap
sum(is.na(tel$hnd_webcap))
#2370 missing values
tel %>% group_by(hnd_webcap) %>% summarise(mean=mean(churn), count=n()) %>% arrange(mean)
#Missing values churn rate is very similar to WC and UNKW values are very smaller, Hence grouping them with WCMB
tel[is.na(tel$hnd_webcap), 'hnd_webcap'] <- "WC"
#Grouping 'UNKW' based on churn rate
tel$hnd_webcap <- plyr::revalue(tel$hnd_webcap, c("UNKW"="WCMB"))
dum_cat <- c(dum_cat, 'hnd_webcap')

#Variable marital
sum(is.na(tel$marital))
#467 missing values
tel %>% group_by(marital) %>% summarise(mean=mean(churn), count=n()) %>% arrange(mean)
#Missing values churn rate is close to S, M & B
tel[is.na(tel$marital), 'marital'] <- "S"
tel$marital <- plyr::revalue(tel$marital, c("S"="B_M_S", "M"="B_M_S", "B"="B_M_S"))
dum_cat <- c(dum_cat, 'marital')

#variable ethnic
sum(is.na(tel$ethnic))
#467 missing values
tel %>% group_by(ethnic) %>% summarise(mean=mean(churn), count=n()) %>% arrange(mean)
#Missing values churn rate is close to G
tel[is.na(tel$ethnic), 'ethnic'] <- "G"
#Grouping based on churn rates
#Grouping S, U, H, F AS F_H_S_U
tel$ethnic <- plyr::revalue(tel$ethnic, c("S"="F_H_S_U", "U"="F_H_S_U", "H"="F_H_S_U", "F"="F_H_S_U"))
#Grouping J, I, R AS I_J_R
tel$ethnic <- plyr::revalue(tel$ethnic, c("J"="I_J_R", "I"="I_J_R", "R"="I_J_R"))
#Grouping B, O, D AS B_D_O
tel$ethnic <- plyr::revalue(tel$ethnic, c("B"="B_D_O", "O"="B_D_O", "D"="B_D_O"))
#Grouping C, Z, X, P, M as C_M_P_X_Z
tel$ethnic <- plyr::revalue(tel$ethnic, c("C"="C_M_P_X_Z", "Z"="C_M_P_X_Z", "X"="C_M_P_X_Z",
                                          "P"="C_M_P_X_Z", "M"="C_M_P_X_Z"))
dum_cat <- c(dum_cat, 'ethnic')

#variable age1
#467 Missing values with churn rate 21.63%
#7296 Default zero age1 values with churn rate 25.94%
#6365 valid age1 values with age lesser than or equal to 36 has churn rate 25.55%
#12332 Other valid age1 values has churn rate 22.08%
#Hence grouping Default zero and valid age1<=36 as 'Def_<=36'
#Hence grouping Missing and valid age1>36 as 'Miss_>36'
tel$age1_c <- 'Miss_>36'
tel[!is.na(tel$age1) & ((tel$age1==0)|(tel$age1<=36)), 'age1_c'] <- 'Def_<=36'
tel %>% group_by(age1_c) %>% summarise(mean=mean(churn), count=n()) %>% arrange(mean)
tel$age1 <- NULL
dum_cat <- c(dum_cat, 'age1_c')


#variable age2
#467 Missing values with churn rate 21.63%
#13596 Default zero age2 values with churn rate 25.03%
#2891 valid age2 values with age lesser than or equal to 32 has churn rate 25.6%
#9506 Other valid age2 values has churn rate 22.08%
#Hence grouping Default zero and valid age2<=32 as 'Def_<=32'
#Hence grouping Missing and valid age2>32 as 'Miss_>32'
tel$age2_c <- 'Miss_>32'
tel[!is.na(tel$age2) & ((tel$age2==0)|(tel$age2<=32)), 'age2_c'] <- 'Def_<=32'
tel %>% group_by(age2_c) %>% summarise(mean=mean(churn), count=n()) %>% arrange(mean)
tel$age2 <- NULL
dum_cat <- c(dum_cat, 'age2_c')

#variable models
tel %>% group_by(models) %>% summarise(mean=mean(churn), count=n()) %>% arrange(mean)
#Models having value 1 with 16423 records and churn rate 25.2%
#Models having value 2 with 6742 records and churn rate 22.66%
#Other values with 3295 records and churn rate 20.58%
tel$models_c <- '1'
tel[tel$models==2, 'models_c'] <- '2'
tel[tel$models>2, 'models_c'] <- '>2'
tel %>% group_by(models_c) %>% summarise(mean=mean(churn), count=n()) %>% arrange(mean)
tel$models <- NULL
dum_cat <- c(dum_cat, 'models_c')


#variable hnd_price
sum(is.na(tel$hnd_price))
#254 missing values
#Rounding the price
tel$hnd_price <- round(tel$hnd_price)
tel %>% group_by(hnd_price) %>% summarise(mean=mean(churn), count=n())
#Grouping based on churn rate
#handset price with less than or equal to 40 has 6411 records and churn rate 30%
#handset price between >40 & 80 has 4874 records and churn rate 24.78%
#handset price between >80 & 150 has 11826 records and churn rate 21.95%
#handset price with missing and >150 has 3349 records and churn rate 18.36%
tel$hnd_price_c <- '81-150'
tel[(tel$hnd_price<=40)& !is.na(tel$hnd_price), 'hnd_price_c'] <- '<=40'
tel[(tel$hnd_price>40) & (tel$hnd_price<=80) & !is.na(tel$hnd_price), 'hnd_price_c'] <- '41-80'
tel[(tel$hnd_price>150) | is.na(tel$hnd_price), 'hnd_price_c'] <- 'Miss_>150'
tel$hnd_price <- NULL
dum_cat <- c(dum_cat, 'hnd_price_c')

#variable actvsubs
tel %>% group_by(actvsubs) %>% summarise(mean=mean(churn), count=n())
#Grouping based on churn rate
#actvsubs with value 1 has 18818 records and churn rate 23.34%
#Others has 1288 records with churn rate 23.76%
#actvsubs with value 2 has 6354 records and churn rate 25.87%
tel$actvsubs_c <- '1_>2'
tel[(tel$actvsubs==2), 'actvsubs_c'] <- '2'
tel$actvsubs <- NULL
dum_cat <- c(dum_cat, 'actvsubs_c')

#variable uniqsubs
tel %>% group_by(uniqsubs) %>% summarise(mean=mean(churn), count=n())
#Grouping based on churn rate
#uniqsubs with value 1 has 16683 records and churn rate 22.45%
#uniqsubs with value 2 has 7200 records and churn rate 26.54%
#Others has 2577 records and churn rate 26.62%
tel$uniqsubs_c <- '1'
tel[(tel$uniqsubs>1), 'uniqsubs_c'] <- '>1'
tel$uniqsubs <- NULL
dum_cat <- c(dum_cat, 'uniqsubs_c')


#variable forgntvl
tel %>% group_by(forgntvl) %>% summarise(mean=mean(churn), count=n())
#More than 90% of the records are having value 0 and there's no much difference in churn rate
#Discarding this variable
tel$forgntvl<-NULL

#variable mtrcycle
tel %>% group_by(mtrcycle) %>% summarise(mean=mean(churn), count=n())
#More than 90% of the records are having value 0 and there's no much difference in churn rate
#Discarding this variable
tel$mtrcycle <- NULL

#variable retdays
#Grouping based on Churn Rate
#Customers with ret days <=45 are  238 records with churn rate 59.24%
#Customers with ret days >45 are  621 records with churn rate 33.98%
#Customers with no ret days are 25601 records with churn rate 23.4%
tel$retdays_c <- 'NA'
tel[!is.na(tel$retdays)&(tel$retdays>45), 'retdays_c'] <- '>45'
tel[!is.na(tel$retdays)&(tel$retdays<=45), 'retdays_c'] <- '<=45'
tel$retdays <- NULL
dum_cat <- c(dum_cat, 'retdays_c')

#variable truck
tel %>% group_by(truck) %>% summarise(mean=mean(churn), count=n())
#Not much of a difference in groups
#Discarding this variable
tel$truck <- NULL

#variable solflag
#In General, the solicit flag always remains 'Y' by default
tel[is.na(tel$solflag), 'solflag'] <- "Y"
tel %>% group_by(solflag) %>% summarise(mean=mean(churn), count=n())
dum_cat <- c(dum_cat, 'solflag')

#variable car_buy
tel %>% group_by(car_buy) %>% summarise(mean=mean(churn), count=n())
tel[is.na(tel$car_buy), 'car_buy'] <- "New"
dum_cat <- c(dum_cat, 'car_buy')

#variable csa
tel %>% group_by(csa) %>% summarise(mean=mean(churn), count=n()) %>% arrange(mean)
#Too Many groups, Discarding this variable
tel$csa <- NULL

#variable div_type
tel %>% group_by(div_type) %>% summarise(mean=mean(churn), count=n()) %>% arrange(mean)
#Missing values mean user may not subscribe to any addityional services
tel$div_type <- as.character(tel$div_type)
tel[is.na(tel$div_type), 'div_type'] <- "NA"
#Not much difference, Discarding this variable
tel$div_type <- NULL

#variable eqpdays, creating category out of the continous variable
#eqpdays with values <=300 has 11663 records with churn rate of 17.1%
#eqpdays with values >300 has 14797 records with churn rate of 29.4%
tel$eqpdays_c <- '>300'
tel[tel$eqpdays<=300, 'eqpdays_c'] <- '<=300'
tel$eqpdays <- NULL
dum_cat <- c(dum_cat, 'eqpdays_c')

#variable months, creating category out of continous variable
#5620 records having months between 6-10 has churn rate of 13.8%
#4584 records having months between 11-13 has churn rate of 34.4%
#13758 records having months between 14-32 has churn rate of 25%
#2498 records having months greater than 32 has churn rate of 21.78%
tel$months_c <- '14-32'
tel[(tel$months<=10), 'months_c'] <- '6-10'
tel[(tel$months>10) & (tel$months<=13), 'months_c'] <- '11-13'
tel[(tel$months>32), 'months_c'] <- '>32'
tel %>% group_by(months_c) %>% summarise(mean=mean(churn), count=n()) %>% arrange(mean)
tel$months <- NULL
dum_cat <- c(dum_cat, 'months_c')

#Feature Engineering
#Deriving overage mou percentage out of total minutes of use
tel %>% mutate(fe_ovrmou_per=ifelse((mou_Mean==0), 100, 100*ovrmou_Mean/mou_Mean)) -> tel

#Deriving voice overage revenue percenatge out of total overrage revenue
tel %>% mutate(fe_vce_ovrrev_per=ifelse((ovrrev_Mean==0),100, 100*(ovrrev_Mean-datovr_Mean)/ovrrev_Mean)) -> tel

#Network quality
#Deriving voice drop percentage
tel %>% mutate(fe_vce_drop_per=ifelse((plcd_vce_Mean==0),100, 100*(plcd_vce_Mean-comp_vce_Mean)/plcd_vce_Mean)) -> tel

#Deriving data drop percentage
tel %>% mutate(fe_dat_drop_per=ifelse((plcd_dat_Mean==0),100, 100*(plcd_dat_Mean-comp_dat_Mean)/plcd_dat_Mean)) -> tel

#Deriving overall drop percentage
tel %>% mutate(fe_comp_drop_per=ifelse(((plcd_dat_Mean+plcd_vce_Mean)==0),0,
                                       100*drop_blk_Mean/(plcd_vce_Mean+plcd_dat_Mean))) -> tel

#Deriving diff percent between last 3 & 6 months mou
tel %>% mutate(fe_diff36mou=ifelse(((avg3mou==0)&(avg6mou==0)), 0,
                                   100*abs(avg6mou-avg3mou)/(avg3mou+avg6mou))) -> tel

#Deriving diff percent between last 3 months & entire life's mou
tel %>% mutate(fe_diff3mou=ifelse(((avg3mou==0)&(avgmou==0)), 0,
                                   100*abs(avgmou-avg3mou)/(avg3mou+avgmou))) -> tel

#Deriving diff percent between last 6 months & entire life's mou
tel %>% mutate(fe_diff6mou=ifelse(((avg6mou==0)&(avgmou==0)), 0,
                                  100*abs(avgmou-avg6mou)/(avg6mou+avgmou))) -> tel


num_col = list(c("mou_Mean", "totmrc_Mean", "rev_Range", "mou_Range", "change_mou", "drop_blk_Mean",
                 "drop_vce_Range", "owylis_vce_Range", "mou_opkv_Range", "totcalls",
                 "custcare_Mean", "callwait_Mean", "iwylis_vce_Mean", "callwait_Range", "ccrndmou_Range",
                 "adjqty", "ovrrev_Mean", "rev_Mean", "ovrmou_Mean", "comp_vce_Mean", "plcd_vce_Mean",
                 "avg3mou", "avgmou", "avg3qty", "avgqty", "avg6mou", "avg6qty", "drop_dat_Mean", "drop_vce_Mean",
                 "adjmou", "totrev", "adjrev", "avgrev", "comp_dat_Mean", "plcd_dat_Mean",
                 "roam_Mean", "recv_sms_Mean", "blck_dat_Mean", "mou_pead_Mean", "opk_dat_Mean",
                 "datovr_Mean", "datovr_Range", "da_Mean", "da_Range"))

#Removing outliers
for (x in num_col[[1]])
{
  if (x!='change_mou')
  {
    p <- min(boxplot.stats(tel[,x])$out)
    q <- quantile(tel[,x], 0.995)
    if (min(p)<q)
      tel[tel[,x]>=q, x] <- median(tel[,x])
    else
      tel[tel[,x]>=p, x] <- median(tel[,x])
  }
  else
  {
    p <- boxplot.stats(tel[,x])$out
    tel[tel[,x] %in% p, x] <- median(tel[,x])
  }
}

#Creating dummy variables with n-1 categories for multicollinearity
for (x in dum_cat)
{
  f=paste('~',x)
  d1 <- dummyVars(formula=f, data=tel, fullRank = TRUE)
  cbind(tel, predict(d1, newdata = tel)) %>% dplyr::select(-x) -> tel
}


data.frame(tel) -> tel1
set.seed(123)
train_ind <- sample(seq_len(nrow(tel1)), size = floor(0.75*nrow(tel)))
train <- tel1[train_ind, ]
test <- tel1[-train_ind, ]
test %>% dplyr::select(-churn) -> X_test
test$churn -> y_test

full_mod <- glm(formula = churn~., data=train)
step_mod <- stepAIC(full_mod, direction = "both")
summary(step_mod)
step_mod$formula

#removing avgmou after 1st round of VIF
#removing actvsubs_c2, totcalls, avgrev, totrev, area.TEXAS_MIDWEST_OHIO, datovr_Mean, datovr_Range
#drop_dat_Mean, fe_vce_drop_per
form = "churn ~ mou_Mean + mou_Range + change_mou + ovrmou_Mean + 
    avgmou + asl_flag + refurb_new + roam_Mean + drop_vce_Mean +
fe_ovrmou_per + fe_comp_drop_per + fe_diff36mou + 
fe_diff3mou + crclscod1B_K_O_T + crclscod1D_E_H_V_Y + prizm_social_one.T + 
area.DALLAS_SOUTHWEST_CHICAGO + area.NEWYORK + area.FLORIDA_PHILY + area.NORTHWEST_ROCKY_MOUNTAIN + 
hnd_webcap.WC + marital.B_M_S + ethnic.C_M_P_X_Z + ethnic.F_H_S_U + 
ethnic.G + ethnic.I_J_R + ethnic.N + `age1_cMiss_>36` + `age2_cMiss_>32` + 
`hnd_price_cMiss_>150` + uniqsubs_c1 + `retdays_c>45` + 
retdays_cNA + `eqpdays_c>300` + `months_c11-13` + `months_c14-32`"

tel$

#VIF is fine after removing variables
max(car::vif(glm(formula = form, data=train)))
names(tel)


mod <- glm(formula = form, family = "binomial", data = train)
pred_prob <- predict(mod, type = 'response', newdata = X_test)
#Plotting ROC curve using pROC package
r1 <- roc(tel$churn, tel$churn_per)
plot.roc(r1, legacy.axes = TRUE, print.auc = TRUE, col = "red")
#AUC-0.67

set.seed(123)
tel$churn_per <- 10
n_fold = 4
auc_tot <- NULL
tel$folds <- createFolds(tel$churn, k=n_fold, list = FALSE)
for (i in seq(n_fold))
{
  mod <- glm(formula = form, family = "binomial", data = tel[tel$folds!=i, ])
  tel[tel$folds==i, 'churn_per'] <- predict(mod, type = 'response', newdata = tel[tel$folds==i, ])
  auc_tot<- c(auc_tot, (roc(tel[tel$folds==i, 'churn'], tel[tel$folds==i, 'churn_per']))$auc)
  if (i==n_fold)
  {
    print(auc_tot)
    print(paste('Mean AUC: ', mean(auc_tot)))
  }
}



helper <- tel[,c('churn', 'churn_per', 'avgrev')]
helper %>% mutate(avgrev_c=ifelse(avgrev>=70, 'High', ifelse(avgrev<=50, 'Low', 'Medium'))) %>%
  dplyr::select(-avgrev) -> helper
helper[,"bucket"] = ntile(-helper[,"churn_per"],n=10)
gaintable = helper %>% group_by(bucket)  %>%
  summarise(total = n(), resp=sum(churn, na.rm = TRUE), percent=resp/total,
            high_resp=sum((avgrev_c=='High')&(churn==1)),
            high_per=sum((avgrev_c=='High')&(churn==1))/sum(avgrev_c=='High'),
            med_resp=sum((avgrev_c=='Medium')&(churn==1)),
            med_per=sum((avgrev_c=='Medium')&(churn==1))/sum(avgrev_c=='Medium'),
            low_resp=sum((avgrev_c=='Low')&(churn==1)),
            low_per=sum((avgrev_c=='Low')&(churn==1))/sum(avgrev_c=='Low')) %>%
  mutate(Cumresp = cumsum(resp), Gain=Cumresp/sum(resp)*100, Lift=100*resp/(sum(resp)/10), Cumlift=Gain/(bucket*(10)))
gaintable
#Gain Chart
plot(c(0,gaintable$bucket), c(0,gaintable$Gain), type="o", ylab="Cumulative gain", xlab="Bucket", col='red')
lines(seq(0,10), seq(0,100,10), type='l', col="blue")
#Lift Chart
plot(gaintable$bucket, gaintable$Lift, type="o", ylab="Lift by Decile", xlab="Bucket", col='red')
lines(rep(100,10), type='l', col="blue")

helper%>%group_by(bucket)%>%summarise(min=min(pred_prob), max=max(pred_prob))

helper %>% group_by(bucket) %>% summarise(mean=mean(y_test))
min(helper[helper$bucket==1, 'pred_prob'])
max(helper[helper$bucket==2, 'pred_prob'])


quantile(tel$churn_per,seq(3)/3)
#Treating 75-100% as high churn, 50-75 as medium churn and less than 50 as Low churn
tel %>% mutate(churn_cat=ifelse((churn_per<=0.22264822),'Low', ifelse((churn_per>=0.32518182), 'High', 'Medium'))) -> tel
quantile(tel$avgrev,seq(3)/3)
tel %>% mutate(rev_cat=ifelse((avgrev<=50),'Low', ifelse((avgrev>=70), 'High', 'Medium'))) -> tel
tel %>% mutate(select_cat=ifelse((((churn_cat=='High')&(rev_cat=='High')) | ((churn_cat=='High')&(rev_cat=='Medium')) |
                         ((churn_cat=='Medium')&(rev_cat=='High'))),'Select', 'NA')) -> tel





helper1 <- tel[tel$select_cat=='Select', c('churn', 'churn_per')]
helper1[,"bucket"] = ntile(-helper1[,"churn_per"],n=10)
gaintable1 = helper1 %>% group_by(bucket)  %>%
  summarise(total = n(), resp=sum(churn, na.rm = TRUE), percent=resp/total) %>%
  mutate(Cumresp = cumsum(resp), Gain=Cumresp/sum(resp)*100, Lift=100*resp/(sum(resp)/10), Cumlift=Gain/(bucket*(10)))
gaintable1


helper2 <- tel[tel$select_cat=='NA', c('churn', 'churn_per')]
helper2[,"bucket"] = ntile(-helper2[,"churn_per"],n=10)
gaintable2 = helper2 %>% group_by(bucket)  %>%
  summarise(total = n(), resp=sum(churn, na.rm = TRUE), percent=resp/total) %>%
  mutate(Cumresp = cumsum(resp), Gain=Cumresp/sum(resp)*100, Lift=100*resp/(sum(resp)/10), Cumlift=Gain/(bucket*(10)))
gaintable2


tel %>% mutate(churn_cat=ifelse((churn_per<=0.1761581),'LowChurn', ifelse((churn_per>0.2721887),
                                                                           'HighChurn', 'MediumChurn'))) -> tel
tel %>% mutate(rev_cat=ifelse((avgrev<=38.97667),'LowRev', ifelse((avgrev>60.77000), 'HighRev', 'MediumRev'))) -> tel

tel$group_cat <- paste(tel$rev_cat, tel$churn_cat)

tel %>% group_by(group_cat) %>% summarize(total_cnt=n(), resp=sum(churn),resp_perc=sum(churn)/n()) %>%
  mutate(tot_per=total_cnt/sum(total_cnt))


