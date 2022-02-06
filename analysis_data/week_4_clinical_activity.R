getwd()
clinic <- read.csv("coad_clinical_data.csv")

# Written Activity

# Question 1
# Categorical variable is a variable that takes on one limited and fixed number of possible values.
# For example, a categorical variable could be a count of number of patients by ethnicity.
# Discrete or continuous variable are obtained by measuring or counting.
# For example, a discrete variable could be a measurement of length or height.

# Question 2
colnames(clinic)
is.na(clinic$colon_polyps_present)
sum(clinic$colon_polyps_present == "")
length(clinic$colon_polyps_present)
# 275 values are empty for colon polyps present of 524 total cases

# Question 3
# Colon polyps present is a categorical variable as it measures whether polyps are present

# Question 4
# Article 1: The differences between fecal microbiota and intestinal fluid microbiota in colon polyps: An observational study
# Article 1 URL: https://oce-ovid-com.libproxy2.usc.edu/article/00005792-202112300-00005/HTML
# Article 1 Summary: There are two general types of intestinal microbiota: intestinal cavity (default) and mucosal. Fresh feces and intestinal fluid was sampled from patients with colon polyps and alpha and beta diversities were calculated. Differences in alpha diversity were not statistically significant, but there were statistical differences in beta diveristy. Thus, there are statistical differences in compositionb etween intestinal microbiota and fecal microbiota in colon polyp patients.

# Article 2: Serrated polyps of the colon and rectum (hyperplastic polyps, sessile serrated adenomas, traditional serrated adenomas, and mixed polyps)—proposal for diagnostic criteria
# Article 2 URL: https://www.proquest.com/docview/750346263/fulltextPDF/42AE1522CDB6419DPQ/1?accountid=14749
# Article 2 Summary: Serrated lesions of polyps show characteristic epigenetic alterations not commonly seen in colorectal adenomas and progress to colorectal carcinoma via the so-called serrated pathway. his group of polyps is comprised not only of hyperplastic polyps, but also of sessile serrated adenomas, traditional serrated adenomas and mixed polyps, showing serrated and “classical” adenomatous features. Diagnostic criteria and nomenclature for these lesions are not uniform and, therefore, somewhat confusing.

# Question 5
# The second variable we chose is age_at_initial_pathologic_diagnosis (categorical variable). 
# This describes the age (years since birth) at which someone was diagnosed with CRC. 

# Question 6
#Hypothesis 1: increased age correlated with increased chance of having colon polyps
#hypothesis 2: having colon polyps is correlated with decreased survival
#hypothesis 3: increased age is correlated with decreased survival

# Question 7
# No values for age_at_initial_pathologic_diagnosis are NA or "", so there is no need to clean the data.
# We will have to clean create a mask for colon_polyps_present to rid  any NA or "" values.

# Create a summary graph of polyp frequency according to age
polyp_table <- table(clinic$colon_polyps_present, clinic$age_at_initial_pathologic_diagnosis)
# Feed in the data into a barplot
barplot(polyp_table, main="Polyps by Age",
        xlab="age", col=c("darkblue","red"),
        legend = rownames(polyp_table), beside=TRUE)

# POLYPS SURVPLOT
# We initialize a 'survival' object first, which contains the data we need.
surv_object <- Surv(time = clinic$days_to_death, 
                    event = clinic$death_event)
# We then create a fit object
polyps_fit <- surv_fit( surv_object ~ clinic$colon_polyps_present, data = clinic)

survplot = ggsurvplot(polyps_fit, 
                      pval=TRUE, 
                      ggtheme = theme(plot.margin = unit(c(1,1,1,1), "cm")), 
                      legend = "right")

p = survplot$plot + 
  theme_bw() +  # changes the appearance to be a bit prettier
  theme(axis.title = element_text(size=20), # increase font sizes
        axis.text = element_text(size=16),
        legend.title = element_text(size=14),
        legend.text = element_text(size=12))

# AGE SURVPLOT
# We initialize a 'survival' object first, which contains the data we need.
surv_object <- Surv(time = clinic$days_to_death, 
                    event = clinic$death_event)
# We then create a fit object
polyps_fit <- surv_fit( surv_object ~ clinic$age_at_initial_pathologic_diagnosis, data = clinic)

survplot = ggsurvplot(polyps_fit, 
                      pval=TRUE, 
                      ggtheme = theme(plot.margin = unit(c(1,1,1,1), "cm")), 
                      legend = "right")

p = survplot$plot + 
  theme_bw() +  # changes the appearance to be a bit prettier
  theme(axis.title = element_text(size=20), # increase font sizes
        axis.text = element_text(size=16),
        legend.title = element_text(size=14),
        legend.text = element_text(size=12))

