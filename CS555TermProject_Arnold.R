##########################
# HICRAN ARNOLD
# MET CS 555 01- Data Analysis and Visualization
# Term Project Code 
############################

################### 1-  INFORMATIO ABOUT THE DATA SET ########################
# Data Set Name : Stroke Prediction Dataset (11 clinical features for predicting stroke events)
# Link to the data source : https://www.kaggle.com/fedesoriano/stroke-prediction-dataset
# Data Se Posted By : fedesoriano ( Kaggle profile: https://www.kaggle.com/fedesoriano )
#Acknowledgements: (Confidential Source) - Use only for educational purposes
#########################  INFORMATION ENDS##################################

##################  2 DATA CLEANUP AND SAMPLING ##############################
################# Data Prep ################
# # Start a fresh R session
# rm(list=ls())
# # open csv file and save the data frame
# data <- read.csv("healthcare-dataset-stroke-data.csv")
# head(data)
# 
# table(data$gender)
# ##checking to see how big my data is
# 
# #check the data length
# numberOfRows <- nrow(data)
# numberOfRows
# 
# # since the data is large we will proceed with cleaning the data and we will sample
# #1000 data points
# 
# ## checking to see if there are dublicate values
# mydata <- sort(unique(data$id), decreasing=TRUE)
# length(mydata)
# 
# # the length are same so we do not have duplicate values 
# myval <- (data$bmi[data$bmi== "N/A"])
# 
# length(myval)
# 
# data[data == "N/A"]  <- NA
# is.na(data$bmi)
# table(is.na(data$bmi))
# 
# #BMI NA removing 201 rows 
# dataBMINAremoved <- data[!(is.na(data$bmi)), ]
# table(is.na(dataBMINAremoved$bmi))
# 
# # removing unknown values in the smoking status
# dataBMINAremoved[dataBMINAremoved == "Unknown"]  <- NA
# is.na(dataBMINAremoved$smoking_status)
# table(is.na(dataBMINAremoved$smoking_status))# we have 4138 columns that marked us unknown 
# 
# 
# #smoking status NA removing 4138 rows 
# dataBMISmokingNAremoved <- dataBMINAremoved[!(is.na(dataBMINAremoved$smoking_status)), ]
# table(is.na(dataBMISmokingNAremoved$smoking_status))# checking to see if we removed the NAs
# 
# # checking to see current size of our data and naming again with a shorter name
# 
# numberOfRows2 <- nrow(dataBMISmokingNAremoved)
# numberOfRows2
# 
# dataCleaned <- dataBMISmokingNAremoved 
# 
# numberOfRows3 <- nrow(dataCleaned)
# numberOfRows3
# ######################### end of cleaning #################
# 
# ######################## sampling #########################
# # For this project recommended sample size is 1000 observations 
# 
# #cannot take a sample larger than the population when 'replace = FALSE'
# 
# # set.seed(1)
# # library(dplyr)
# # 
# # table(dataCleaned$gender)
# # strat_sample <- dataCleaned %>%
# #   group_by(dataCleaned$gender)%>%
# #   sample_n(nrow(dataCleaned),size=500)
# 
# ###########
# set.seed(255555) # to get to same result again 
# 
# sampledData <- dataCleaned[sample(nrow(dataCleaned), size = 1000, replace = FALSE), ]
# 
# ## checking to see if there are dublicate values
# mydata <- sort(unique(sampledData$id), decreasing=TRUE)
# length(mydata)
# table(is.na(sampledData$bmi))
# 
# getwd()
# 
# write.csv(sampledData, 
#           "C:\\Users\\Hicran\\Documents\\homework\\term project\\sampledData2.csv",
#           row.names = FALSE)
################################

############################ 2 END OF THE CLEANING CODE ########################



######################## 3 - READING THE DATA ###############################
# Start a fresh R session
rm(list=ls())
# turn off the scientific notation
options(scipen = 100)
# open csv file and save the data frame
data <- read.csv("sampledData2.csv")
######################### READING DATA ENDS ################################# 


######################### 4- SUMMARASING THE DATA #########################

addmargins(table(Gender=data$gender, Stroke=data$stroke))

#Graphically:
tt <-table(Gender=data$gender, Stroke=data$stroke)
# Stacked bar-charts -  CONDITIONAL distributions
ptt <-prop.table(t(tt), margin=2)
#barplot(t(ptt), beside=TRUE, legend=TRUE)
barplot(ptt, beside=TRUE, legend=TRUE, ylim = c(0, 1))

### b create a tible table
myTable2 <- addmargins(table(data$stroke, data$gender, dnn = c("Stroke", "Gender")))
DF <- data.frame(unclass(myTable2))

colnames(DF) <- c("Females", "Males", "Total")
DFmA1 <- DF$Males[1]
DFmA2 <- DF$Males[2]
DFWO1 <- DF$Females[1]
DFWO2 <- DF$Females[2]
names <- c("Stroke_0", "Stroke_1")

Stroke_1 <- c(DFmA2 , DFWO2)
Stroke_0 <- c(DFmA1 ,DFWO1)
population_description <- c("Male", "Female")
df <- data.frame( population_description, Stroke_1, Stroke_0)
df$sampleSize <- df$Stroke_1+df$Stroke_0
df$Sample.proportion <- (df$Stroke_1/ df$sampleSize)

# convert to a table
library(data.table)
setDT(df)
print (df)

femaleprop<- (28/597)*100
maleprop<- (23/375)*100

######################## 3 END OF SUMMARASING THE DATA########################


######################## 5 GENDER VS STROKE EVENT  ###############################

# two-sample test proportions
# estimated risk difference 
RD <-  maleprop - femaleprop
RD 

# estimated risk ratio 
RR <- maleprop/femaleprop



proptestForGender <-prop.test(c(23,28),c(375,625), 
                                                    alternative="two.sided",  
                                                    conf.level = 0.95, correct = FALSE)
proptestForGender
zval <- sqrt(proptestForGender[["statistic"]][["X-squared"]])
zval
pval <- proptestForGender[["p.value"]]

# population proportion
p <- ((28+23)/(625+375))
p


# we can double check our calculation by manually calculating
zmanualCal<- (0.06133333-0.04480000)/ sqrt(p *(1-p)*((1/625)+ (1/375)))
######################## 5 ENDS GENDER VS STROKE EVENT #####################

####################### 6 AGE AND BMI VS STROKE EVENT #######################
#Is there any association in between stroke,age and bmi level ? 
# 
# summary
summary(data$age[data$stroke ==1])
summary(data$age[data$stroke ==0])

label=c("no_stroke", "Had_stroke")
boxplot(data$age~data$stroke,data=data, 
        main = "Stroke Summary by Age",
        xlab = "Stroke", 
        ylab = "Age", col = c("azure3","firebrick1" ),
        names= label
)

# bmi summary

summary(data$bmi[data$stroke ==1])

summary(data$bmi[data$stroke ==0])

# labels for Axis

label=c("no_stroke", "Had_stroke")
boxplot(data$bmi~data$stroke,data=data, 
        main = "Stroke Summary by BMI",
        xlab = "Stroke", 
        ylab = "BMI", col = c("azure3","firebrick1" ),
        names= label
)




library(aod)
library(pROC)

#data$male <- ifelse(data$gender == "Male",1,0) # this does not seems nessary I ended up having the same result
m2 <- glm(formula = data$stroke ~ 
           + data$bmi
          +data$age, family = binomial)


wald.test(b=coef(m2), Sigma = vcov(m2), Terms = 2:3)
summary(m2)
# ORs per 1 unit increase
exp(cbind(OR = coef(m2), confint.default(m2)))


data$ProbofTL3<-predict(m2, type="response") # Vector of P(getting pizza)

g2 <-roc(data$stroke~data$ProbofTL3)
g2
# using model 2
data$prob2<-predict(m2,type=c("response"))
g2 <- roc(data$stroke ~ data$prob2)
plot(1-g2$specificities, g2$sensitivities, type = "l",
     xlab = "1 - Specificity", ylab = "Sensitivity", main = "ROC Curve\nArea under the curve =  0.8175")
abline(a=0,b=1)
grid()


############################# 6 END OF AGE AND BMI VS STROKE EVENT#############






