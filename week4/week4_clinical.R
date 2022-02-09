# download BioManager/TCGAbiolinks
if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install(version = "3.13")
if(!require(TCGAbiolinks)) BiocManager::install("TCGAbiolinks")

# set directory
getwd()
setwd("Documents/USC/Classes/QBIO/qbio_data_analysis_joshl/analysis_data")


clin_query <- GDCquery(project = "TCGA-COAD", data.category = "Clinical", file.type = "xml")
# Only use this line ONCE! Comment out after you have downloaded the data. 
# GDCdownload(clin_query)
clinic <- GDCprepare_clinic(clin_query, clinical.info = "patient")
# Just adding an underscore between follow and up
names(clinic)[names(clinic)=="days_to_last_followup"] <- "days_to_last_follow_up"

# Exercise 1.1: look at the clinic object here
# str() function in R Language is used for compactly displaying the internal 
# structure of a R object 
str(clinic)
# head() function in R Language is used to get the first parts of a vector, 
# matrix, table, data frame or function
head(clinic)

# Exercise 1.2: Look at the clinic object using colnames
# Access any column using the $ syntax, for example clinic$vital_status
colnames(clinic)

# Exercise 2.1: scatterplot of age vs. weight
plot(clinic$age_at_initial_pathologic_diagnosis, clinic$weight,
     xlab = "age", ylab = "weight", main = "Demographics")

# Exercise 2.2: boxplot of age (y-axis) vs. race (x-axis)
unique(clinic$race_list)
par(mar=c(10,1,1,1))
boxplot(clinic$age_at_initial_pathologic_diagnosis ~ clinic$race_list,  
        las = 2, 
        cex.axis = 0.5,
        xlab = "race")

# Exercise 2.3: Replace "" with "NO DATA"
clinic$race_list = as.character(clinic$race_list)
mask <- clinic$race_list == ""
clinic$race_list <- ifelse(mask, "NO DATA", clinic$race_list)
clinic$race_list

# Exercise 2.4: Age Summary Statistics
# find the min, max, mean, and median ages
# also, run the summary() function
min(clinic$age_at_initial_pathologic_diagnosis)
max(clinic$age_at_initial_pathologic_diagnosis)
mean(clinic$age_at_initial_pathologic_diagnosis)
median(clinic$age_at_initial_pathologic_diagnosis)
summary(clinic$age_at_initial_pathologic_diagnosis)

# Exercise 2.5: “young” as younger than 50 years old, 
# and “old” as greater than or equal to 50 years old
young = clinic$age_at_initial_pathologic_diagnosis < 50
sum(young)
old = clinic$age_at_initial_pathologic_diagnosis >= 50
sum(old)

# Exercise 2.6: patient ids that correspond to young and old
young_patient_ids = ifelse(young == TRUE, clinic$bcr_patient_barcode,"")
  old_patient_ids = ifelse(old == TRUE, clinic$bcr_patient_barcode,"")
  
# Exercise 2.7: age_category to store young/old classification
clinic$age_category = ifelse(young == TRUE, "young", "old")

# Exercise 2.8
clinic[1,]
# leaving the column value blank will return information in an entire row.

# Exercise 2.9
# create two new data frames, young_clinic and old_clinic
# hint: use the syntax from before
young_clinic = data.frame()
young_clinic <- c(clinic$bcr_patient_barcode, young == TRUE)
young_clinic <- c()
View(young_clinic)
old_clinic = clinic$age_at_initial_pathologic_diagnosis >= 50

# Exercise 2.10
young_clinic_one_line =
identical(dim(young_clinic), dim(young_clinic_one_line))

# Load survival and surminer packages
install.packages("survival")
install.packages("survminer")
library("survival")
library("survminer")

# Exercise 3.1
clinic_cleaned <- clinic
is.na(clinic_cleaned$days_to_death)
clinic_cleaned$days_to_death <- ifelse(is.na(clinic_cleaned$days_to_death), 
       clinic_cleaned$days_to_last_follow_up, 
       clinic_cleaned$days_to_death)
clinic_cleaned$days_to_death

# Exercise 3.2
# Create a death_event 
clinic_cleaned$death_event <- c(ifelse(clinic_cleaned$vital_status == "Alive", 0, 1))

# We initialize a 'survival' object first, which contains the data we need.
surv_object <- Surv(time = clinic_cleaned$days_to_death, 
                    event = clinic_cleaned$death_event)

# We then create a fit object
race_fit <- surv_fit( surv_object ~ clinic_cleaned$race_list, data = clinic_cleaned )

#the ggtheme and legend arguments are for formatting. 
# Feel free to play around with the margins and legend placement
survplot = ggsurvplot(race_fit, 
                      pval=TRUE, 
                      ggtheme = theme(plot.margin = unit(c(1,1,1,1), "cm")), 
                      legend = "right")

p = survplot$plot + 
  theme_bw() +  # changes the appearance to be a bit prettier
  theme(axis.title = element_text(size=20), # increase font sizes
        axis.text = element_text(size=16),
        legend.title = element_text(size=14),
        legend.text = element_text(size=12))
p

# save the plot as a png
# if you want to present the data, change the strata labels!
ggsave("Documents/USC/QBIO/qbio_data_analysis_joshl/week4_clinical/kmplot_by_race.png", plot = p, width = 12, height = 9)

# Exercise 3.3
# The plot shows that Asian patients have the highest survival rate, and 
# African Americans have the lowest survival rate. However, it seems that
# there is not enough data to affirm the accuracy of survival rate for Asians
# as the sample size seems to be smaller as opposed to other ethic groups.

# Exercise 4.1
write.csv(clinic, "Users/joshlu/Desktop/QBIO/qbio_data_analysis_joshl/week4_clinical/coad_clinical_data.csv", row.names = F)
clinic_read_in <- read.csv("Users/joshlu/Desktop/QBIO/qbio_data_analysis_joshl/week4_clinical/coad_clinical_data.csv")