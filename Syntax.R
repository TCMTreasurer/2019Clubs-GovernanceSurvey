#### Prepare Workspace ####

#--- Install packages ---#
install.packages("ggpubr")
install.packages("gplots")
install.packages("graphics")
install.packages("effsize")
install.packages("xlsx")
install.packages("vcd")
install.packages("car")

#--- Load packages ---#
library(tidyverse)
library(ggplot2)
library(dplyr)
library(stringr)
library(ggpubr)
library(gplots)
library(graphics)
library(e1071)
library(effsize)
library(xlsx)
library(vcd)
library(car)
library(DescTools)

#--- Install original functions ---#
source("list.frequencies.R")
source("range.to.integer.R")
source("count.responses.R")

#--- Read data ---#
dataset<-read.csv("Clubs & Governance Survey 2019.csv")
View(dataset)

#### Prepare Data ####

#--- Remove ancillary rows---#
dataset$timestamp<-NULL
dataset$tcm.attendance.comments<-NULL
dataset$tcm.engagement.comments<-NULL

#--- Convert ethnicity responses to standardized form ---#
dataset<-mutate(dataset, standardized.ethnicity=recode(ethnicity,
                        "Indian" = "Asian", "Middle Eastern" = "Asian", "South Asian" = "Asian",
                        "South east asian" = "Asian", "Southeast asian" = "Asian",
                        "South Asian " = "Asian", "Hispanic, Latino, or of Spanish origin" = "Hispanic"))

#--- Create variable for majority ethnicity ---#
dataset<-mutate(dataset, is.white=recode(standardized.ethnicity,
                         "White" = "Yes", "Asian" = "No", "Black" = "No", "Hispanic" = "No",
                         "Indigenous" = "No", "Mixed Race" = "No"))

#--- Create variable for upper and lower years ---#
dataset<-mutate(dataset, year.level=recode(social.year,
                         "1 (2T2)" = "Lower Year", "2 (2T1)" = "Lower Year", "3 (2T0)" = "Upper Year",
                         "4 (1T9)" = "Upper Year", "5+" = "Upper Year"))

#--- Create variable for numeric year ---#
dataset<-mutate(dataset, numeric.year=recode(social.year,
                         "1 (2T2)" = 1, "2 (2T1)" = 2, "3 (2T0)" = 3, "4 (1T9)" = 4, "5+" = 5))

#--- Create variable for study intensity ---#
dataset<-mutate(dataset, study.intensity=recode(study,
                         "Commerce" = "Soft Major", "Divinity" = "Soft Major", "Humanities" = "Soft Major",
                         "Humanities & Life Sciences" = "Soft Major", "Life Sciences" = "Hard Major",
                         "Mathematics/Computer Sciences" = "Hard Major", "Physical Sciences" = "Hard Major",
                         "Social Sciences" = "Soft Major"))

#--- Create variable for males and non-males ---#
dataset<-mutate(dataset, is.male=recode(gender,
                         "Female" = "No", "Male" = "Yes", "Non-Binary" = "No", "Prefer not to answer" = "No"))

#--- Convert TCM attendance responses into standardized ranges---#
dataset<-mutate(dataset, tcm.attendance.range=recode(tcm.attendance, "0" = "0 to 0", "1" = "1 to 1"))

#--- Convert TCM attendance responses from range to random number in range---#
dataset<-mutate(dataset, tcm.attendance.estimate=sapply(dataset$tcm.attendance.range, function(x) {
    vals<-as.integer(unlist(strsplit(as.character(x), "\\s+to\\s+")))
    sample(vals[1]:vals[2], 1, replace=TRUE)
  }))

#--- Confirm normal distribution for continuous variables: TCM engagement ---#
ggplot(data=dataset, aes(x=engagement)) + geom_histogram()
ggqqplot(dataset$engagement)
shapiro.test(dataset$engagement)
skewness(dataset$engagement)

#--- Confirm normal distribution for continuous variables: TCM attendance ---#
ggplot(data=dataset, aes(x=tcm.attendance.estimate)) + geom_histogram()
ggqqplot(dataset$tcm.attendance.estimate)
shapiro.test(dataset$tcm.attendance.estimate)
skewness(dataset$tcm.attendance.estimate)

#--- Reduce some skew in TCM attendance ---#
dataset$tcm.attendance.estimate.log<-log10(dataset$tcm.attendance.estimate + 1)
ggplot(data=dataset, aes(x=tcm.attendance.estimate.log)) + geom_histogram()
ggqqplot(dataset$tcm.attendance.estimate.log)
skewness(dataset$tcm.attendance.estimate.log)

#--- Create a variable for number of clubs known and attended ---#
dataset<-mutate(dataset, clubs.known.number=sapply(dataset$clubs.known, function(x) {
  vals<-unlist(strsplit(as.character(x), ",\\s+"))
  length(vals)
  }))
dataset<-mutate(dataset, clubs.attended.number=sapply(dataset$clubs.attended, function(x) {
  vals<-unlist(strsplit(as.character(x), ",\\s+"))
  length(vals)
}))

#--- Create variables for each information source ---#
dataset$information.source.facebook<-ifelse(grepl("Facebook", dataset$information.source), "Yes", "No")
dataset$information.source.email<-ifelse(grepl("Email", dataset$information.source), "Yes", "No")
dataset$information.source.other<-ifelse(grepl("Friends", dataset$information.source), "Yes",
                                  ifelse(grepl("Word", dataset$information.source), "Yes", "No"))

#--- Create variable for 2T2 Facebook Group ---#
dataset$information.source.2t2<-ifelse(grepl("2T2", dataset$information.source), "Yes", "No")

#### Get Demographics ####

#--- Get sample size---#
nrow(dataset)

#--- Get breakdown for categorical variables ---#
summary(dataset$social.year)
summary(dataset$study)
summary(dataset$residence.status)
summary(dataset$international.status)
summary(dataset$gender)
summary(dataset$standardized.ethnicity)

#--- Get proportions for categorical variables ---#
round(prop.table(table(dataset$social.year)) * 100)
round(prop.table(table(dataset$study)) * 100)
round(prop.table(table(dataset$residence.status)) * 100)
round(prop.table(table(dataset$international.status)) * 100)
round(prop.table(table(dataset$gender)) * 100)
round(prop.table(table(dataset$standardized.ethnicity)) * 100)

#--- Get mean and standard deviation for continuous variables ---#
mean(dataset$engagement)
sd(dataset$engagement)
mean(dataset$tcm.attendance.estimate)
sd(dataset$tcm.attendance.estimate)

#### Get Response Frequencies ####

#--- Calculate most known clubs ---#
list.frequencies(dataset$clubs.known)
round((list.frequencies(dataset$clubs.known)/90)*100)

#--- Calculate most attended clubs ---#
list.frequencies(dataset$clubs.attended)
round((list.frequencies(dataset$clubs.attended)/90)*100)

#--- Calculate most important clubs ---#
list.frequencies(dataset$clubs.important)
round((list.frequencies(dataset$clubs.important)/90)*100)

#--- Calculate most common reasons for attending TCMs ---#
sort(table(dataset$tcm.attendance.reasons), decreasing=TRUE)
round((sort(table(dataset$tcm.attendance.reasons), decreasing=TRUE)/90)*100)

#--- Calculate most desired TCM start time ---#
sort(table(dataset$tcm.ideal.time), decreasing=TRUE)
round((sort(table(dataset$tcm.ideal.time), decreasing=TRUE)/90)*100)

#--- Calculate most common sources of information ---#
list.frequencies(dataset$information.source)
round((list.frequencies(dataset$information.source)/90)*100)
round((table(dataset$information.source.facebook)/90)*100)
round((table(dataset$information.source.email)/90)*100)
round((table(dataset$information.source.other)/90)*100)

#--- Calculate need for more event emails ---#
table(dataset$more.emails.events)
round((sort(table(dataset$more.emails.events), decreasing=TRUE)/90)*100)

#--- Calculate need for more governance emails ---#
table(dataset$more.emails.governance)
round((sort(table(dataset$more.emails.governance), decreasing=TRUE)/90)*100)

#### Run Main Analysis ####

#--- ANOVA for engagement level and social year ---#
ggboxplot(dataset, x="social.year", y="engagement",
          color="social.year", palette=c("#00AFBB", "#E7B800", "#FC4E07", "#2FB25F", "#00AFBB"),
          order=c("1 (2T2)", "2 (2T1)", "3 (2T0)", "4 (1T9)", "5+"),
          ylab="Engagement", xlab="Social Year")
summary(aov(engagement ~ social.year, data=dataset))
group_by(dataset, social.year) %>%
  summarise(
    count=n(),
    mean=mean(engagement, na.rm = TRUE),
    sd=sd(engagement, na.rm = TRUE))

#--- ANOVA for TCM attendance and social year ---#
ggboxplot(dataset, x="social.year", y="tcm.attendance.estimate",
          color="social.year", palette=c("#00AFBB", "#E7B800", "#FC4E07", "#2FB25F", "#00AFBB"),
          order=c("1 (2T2)", "2 (2T1)", "3 (2T0)", "4 (1T9)", "5+"),
          ylab="TCMs Attended", xlab="Social Year")
summary(aov(tcm.attendance.estimate.log ~ social.year, data=dataset))
group_by(dataset, social.year) %>%
  summarise(
    count=n(),
    mean=mean(tcm.attendance.estimate, na.rm = TRUE),
    sd=sd(tcm.attendance.estimate, na.rm = TRUE))

#--- Spearman's rank correlation for engagement level and social year ---#
cor.test(dataset$engagement, dataset$numeric.year,  method = "spearman")
ggplot(dataset, aes(x=numeric.year, y=engagement)) + geom_smooth(method=lm)

#--- Spearman's rank correlation for TCM attendance and social year ---#
cor.test(dataset$tcm.attendance.estimate.log, dataset$numeric.year,  method = "spearman")
ggplot(dataset, aes(x=numeric.year, y=tcm.attendance.estimate)) + geom_smooth(method=lm)

#--- t-test for engagement level and social year (lower or lower) ---#
ggboxplot(dataset, x="year.level", y="engagement",
          color="year.level", palette=c("#00AFBB", "#FC4E07"),
          order=c("Lower Year", "Upper Year"),
          ylab="Engagement", xlab="Year Level")
t.test(engagement ~ year.level, data=dataset, var.equal=TRUE)
group_by(dataset, year.level) %>%
  summarise(
    count=n(),
    mean=mean(engagement, na.rm = TRUE),
    sd=sd(engagement, na.rm = TRUE))

#--- t-test for TCM attendance and social year (lower or lower) ---#
ggboxplot(dataset, x="year.level", y="tcm.attendance.estimate",
          color="year.level", palette=c("#00AFBB", "#FC4E07"),
          order=c("Lower Year", "Upper Year"),
          ylab="TCMs Attended", xlab="Year Level")
t.test(tcm.attendance.estimate.log ~ year.level, data=dataset, var.equal=TRUE)
group_by(dataset, year.level) %>%
  summarise(
    count=n(),
    mean=mean(tcm.attendance.estimate, na.rm = TRUE),
    sd=sd(tcm.attendance.estimate, na.rm = TRUE))

#--- ANOVA for engagement level and field of study ---#
ggboxplot(dataset, x="study", y="engagement",
          color="study", palette=c("#00AFBB", "#E7B800", "#FC4E07", "#2FB25F",
                                     "#55509E","#00AFBB", "#E7B800", "#FC4E07"),
          order=c("Social Sciences", "Humanities", "Life Sciences", "Commerce",
                  "Mathematics/Computer Sciences", "Physical Sciences", "Divinity"),
          ylab="Engagement", xlab="Field of Study")
summary(aov(engagement ~ study, data=dataset))
group_by(dataset, study) %>%
  summarise(
    count=n(),
    mean=mean(engagement, na.rm = TRUE),
    sd=sd(engagement, na.rm = TRUE))

#--- ANOVA for TCM attendance and field of study ---#
ggboxplot(dataset, x="study", y="tcm.attendance.estimate",
          color="study", palette=c("#00AFBB", "#E7B800", "#FC4E07", "#2FB25F",
                                   "#55509E","#00AFBB", "#E7B800", "#FC4E07"),
          order=c("Social Sciences", "Humanities", "Life Sciences", "Commerce",
                  "Mathematics/Computer Sciences", "Physical Sciences", "Divinity"),
          ylab="TCMs Attended", xlab="Field of Study")
summary(aov(tcm.attendance.estimate.log ~ study, data=dataset))
group_by(dataset, study) %>%
  summarise(
    count=n(),
    mean=mean(tcm.attendance.estimate, na.rm = TRUE),
    sd=sd(tcm.attendance.estimate, na.rm = TRUE))

#--- t-test for engagement level and study intensity ---#
ggboxplot(dataset, x="study.intensity", y="engagement",
          color="study.intensity", palette=c("#00AFBB", "#FC4E07"),
          order=c("Soft Major", "Hard Major"),
          ylab="Engagement", xlab="Study Intensity")
t.test(engagement ~ study.intensity, data=dataset, var.equal=TRUE)
group_by(dataset, study.intensity) %>%
  summarise(
    count=n(),
    mean=mean(engagement, na.rm = TRUE),
    sd=sd(engagement, na.rm = TRUE))

#--- t-test for TCM attendance and study intensity ---#
ggboxplot(dataset, x="study.intensity", y="tcm.attendance.estimate",
          color="study.intensity", palette=c("#00AFBB", "#FC4E07"),
          order=c("Soft Major", "Hard Major"),
          ylab="TCMs Attended", xlab="Study Intensity")
t.test(tcm.attendance.estimate.log ~ study.intensity, data=dataset, var.equal=TRUE)
group_by(dataset, study.intensity) %>%
  summarise(
    count=n(),
    mean=mean(tcm.attendance.estimate, na.rm = TRUE),
    sd=sd(tcm.attendance.estimate, na.rm = TRUE))

#--- t-test for engagement level and residence status ---#
ggboxplot(dataset, x="residence.status", y="engagement",
          color="residence.status", palette=c("#00AFBB", "#FC4E07"),
          order=c("Resident", "Non-Resident"),
          ylab="Engagement", xlab="Residence Status")
t.test(engagement ~ residence.status, data=dataset, var.equal=TRUE)
group_by(dataset, residence.status) %>%
  summarise(
    count=n(),
    mean=mean(engagement, na.rm = TRUE),
    sd=sd(engagement, na.rm = TRUE))
cohen.d(dataset$engagement, dataset$residence.status)

#--- t-test for TCM attendance and residence status ---#
ggboxplot(dataset, x="residence.status", y="tcm.attendance.estimate",
          color="residence.status", palette=c("#00AFBB", "#FC4E07"),
          order=c("Resident", "Non-Resident"),
          ylab="TCMs Attended", xlab="Residence Status")
t.test(tcm.attendance.estimate.log ~ residence.status, data=dataset, var.equal=TRUE)
group_by(dataset, residence.status) %>%
  summarise(
    count=n(),
    mean=mean(tcm.attendance.estimate, na.rm = TRUE),
    sd=sd(tcm.attendance.estimate, na.rm = TRUE))
cohen.d(dataset$tcm.attendance.estimate.log, dataset$residence.status)

#--- t-test for engagement level and any residence ---#
ggboxplot(dataset, x="any.residence", y="engagement",
          color="any.residence", palette=c("#00AFBB", "#FC4E07"),
          order=c("Yes", "No, I have never lived in residence"),
          ylab="Engagement", xlab="Any Residence")
t.test(engagement ~ any.residence, data=dataset, var.equal=TRUE)
group_by(dataset, any.residence) %>%
  summarise(
    count=n(),
    mean=mean(engagement, na.rm = TRUE),
    sd=sd(engagement, na.rm = TRUE))

#--- t-test for TCM attendance and any residence ---#
ggboxplot(dataset, x="any.residence", y="tcm.attendance.estimate",
          color="any.residence", palette=c("#00AFBB", "#FC4E07"),
          order=c("Yes", "No, I have never lived in residence"),
          ylab="TCMs Attended", xlab="Any Residence")
t.test(tcm.attendance.estimate.log ~ any.residence, data=dataset, var.equal=TRUE)
group_by(dataset, any.residence) %>%
  summarise(
    count=n(),
    mean=mean(tcm.attendance.estimate, na.rm = TRUE),
    sd=sd(tcm.attendance.estimate, na.rm = TRUE))

#--- t-test for engagement level and international status ---#
ggboxplot(dataset, x="international.status", y="engagement",
          color="international.status", palette=c("#00AFBB", "#FC4E07"),
          order=c("No", "Yes"),
          ylab="Engagement", xlab="International Status")
t.test(engagement ~ international.status, data=dataset, var.equal=TRUE)
group_by(dataset, international.status) %>%
  summarise(
    count=n(),
    mean=mean(engagement, na.rm = TRUE),
    sd=sd(engagement, na.rm = TRUE))

#--- t-test for TCM attendance and international status ---#
ggboxplot(dataset, x="international.status", y="tcm.attendance.estimate",
          color="international.status", palette=c("#00AFBB", "#FC4E07"),
          order=c("No", "Yes"),
          ylab="TCMs Attended", xlab="International Status")
t.test(tcm.attendance.estimate.log ~ international.status, data=dataset, var.equal=TRUE)
group_by(dataset, international.status) %>%
  summarise(
    count=n(),
    mean=mean(tcm.attendance.estimate, na.rm = TRUE),
    sd=sd(tcm.attendance.estimate, na.rm = TRUE))
cohen.d(dataset$tcm.attendance.estimate.log, dataset$international.status)

#--- ANOVA for engagement level and gender ---#
ggboxplot(dataset, x="gender", y="engagement",
          color="gender", palette=c("#00AFBB", "#E7B800", "#FC4E07"),
          order=c("Male", "Female", "Non-Binary"),
          ylab="Engagement", xlab="Gender")
summary(aov(engagement ~ gender, data=dataset))
group_by(dataset, gender) %>%
  summarise(
    count=n(),
    mean=mean(engagement, na.rm = TRUE),
    sd=sd(engagement, na.rm = TRUE))

#--- ANOVA for TCM attendance and gender ---#
ggboxplot(dataset, x="gender", y="tcm.attendance.estimate",
          color="gender", palette=c("#00AFBB", "#E7B800", "#FC4E07"),
          order=c("Male", "Female", "Non-Binary"),
          ylab="TCMs Attended", xlab="Gender")
summary(aov(tcm.attendance.estimate.log ~ gender, data=dataset))
group_by(dataset, gender) %>%
  summarise(
    count=n(),
    mean=mean(tcm.attendance.estimate, na.rm = TRUE),
    sd=sd(tcm.attendance.estimate, na.rm = TRUE))

#--- t-test engagement level and binary gender --#
ggboxplot(dataset, x="is.male", y="engagement",
          color="is.male", palette=c("#00AFBB", "#FC4E07"),
          order=c("No", "Yes"),
          ylab="Engagement", xlab="Male")
t.test(engagement ~ is.male, data=dataset, var.equal=TRUE)
group_by(dataset, is.male) %>%
  summarise(
    count=n(),
    mean=mean(engagement, na.rm = TRUE),
    sd=sd(engagement, na.rm = TRUE))

#--- t-test TCM attendance and binary gender --#
ggboxplot(dataset, x="is.male", y="tcm.attendance.estimate",
          color="is.male", palette=c("#00AFBB", "#FC4E07"),
          order=c("No", "Yes"),
          ylab="TCMs Attended", xlab="Male")
t.test(tcm.attendance.estimate.log ~ is.male, data=dataset, var.equal=TRUE)
group_by(dataset, is.male) %>%
  summarise(
    count=n(),
    mean=mean(tcm.attendance.estimate, na.rm = TRUE),
    sd=sd(tcm.attendance.estimate, na.rm = TRUE))
cohen.d(dataset$tcm.attendance.estimate.log, dataset$is.male)

#--- ANOVA for engagement level and ethnicity ---#
ggboxplot(dataset, x="standardized.ethnicity", y="engagement",
          color="standardized.ethnicity", palette=c("#00AFBB", "#E7B800", "#FC4E07",
                                                    "#2FB25F","#55509E","#00AFBB"),
          order=c("Asian", "Black", "Hispanic", 
                  "Indigenous", "Mixed Race", "White"),
          ylab="Engagement", xlab="Ethnicity")
summary(aov(engagement ~ standardized.ethnicity, data=dataset))
group_by(dataset, standardized.ethnicity) %>%
  summarise(
    count=n(),
    mean=mean(engagement, na.rm = TRUE),
    sd=sd(engagement, na.rm = TRUE))
TukeyHSD(aov(engagement ~ standardized.ethnicity, data=dataset))
EtaSq(aov(engagement ~ standardized.ethnicity, data=dataset))

#--- ANOVA for TCM attendance and ethnicity ---#
ggboxplot(dataset, x="standardized.ethnicity", y="tcm.attendance.estimate",
          color="standardized.ethnicity", palette=c("#00AFBB", "#E7B800", "#FC4E07",
                                                    "#2FB25F","#55509E","#00AFBB"),
          order=c("Asian", "Black", "Hispanic", 
                  "Indigenous", "Mixed Race", "White"),
          ylab="TCMs Attended", xlab="Ethnicity")
summary(aov(tcm.attendance.estimate.log ~ standardized.ethnicity, data=dataset))
group_by(dataset, standardized.ethnicity) %>%
  summarise(
    count=n(),
    mean=mean(tcm.attendance.estimate, na.rm = TRUE),
    sd=sd(tcm.attendance.estimate, na.rm = TRUE))
TukeyHSD(aov(tcm.attendance.estimate.log ~ standardized.ethnicity, data=dataset))
EtaSq(aov(tcm.attendance.estimate.log ~ standardized.ethnicity, data=dataset))

#--- t-test for engagement level and majority ethnicity ---#
ggboxplot(dataset, x="is.white", y="engagement",
          color="is.white", palette=c("#00AFBB", "#FC4E07"),
          order=c("Yes", "No"),
          ylab="Engagement", xlab="Majority Ethnicity")
t.test(engagement ~ is.white, data=dataset, var.equal=TRUE)
group_by(dataset, is.white) %>%
  summarise(
    count=n(),
    mean=mean(engagement, na.rm = TRUE),
    sd=sd(engagement, na.rm = TRUE))
cohen.d(dataset$engagement, dataset$is.white)

#--- t-test for TCM attendance and majority ethnicity ---#
ggboxplot(dataset, x="is.white", y="tcm.attendance.estimate",
          color="is.white", palette=c("#00AFBB", "#FC4E07"),
          order=c("Yes", "No"),
          ylab="TCMs Attended", xlab="Majority Ethnicity")
t.test(tcm.attendance.estimate.log ~ is.white, data=dataset, var.equal=TRUE)
group_by(dataset, is.white) %>%
  summarise(
    count=n(),
    mean=mean(tcm.attendance.estimate, na.rm = TRUE),
    sd=sd(tcm.attendance.estimate, na.rm = TRUE))
cohen.d(dataset$tcm.attendance.estimate.log, dataset$is.white)

#### Run Follow-Up Analysis ####

#--- Chi-square test for independence for residence status and majority ethnicity ---#
chisq.test(table(dataset$residence.status, dataset$is.white))
mosaicplot(table(dataset$residence.status, dataset$is.white), shade=TRUE)

#--- t-test for engagement and information source: Facebook ---#
ggboxplot(dataset, x="information.source.facebook", y="engagement",
          color="information.source.facebook", palette=c("#00AFBB", "#FC4E07"),
          order=c("Yes", "No"),
          ylab="Engagement", xlab="Uses Facebook for Informaiton")
t.test(engagement ~ information.source.facebook, data=dataset, var.equal=TRUE)
group_by(dataset, information.source.facebook) %>%
  summarise(
    count=n(),
    mean=mean(engagement, na.rm = TRUE),
    sd=sd(engagement, na.rm = TRUE))
cohen.d(dataset$engagement, dataset$information.source.facebook)

#--- t-test for engagement and information source: Facebook 2T2 Group ---#
ggboxplot(dataset, x="information.source.2t2", y="engagement",
          color="information.source.2t2", palette=c("#00AFBB", "#FC4E07"),
          order=c("Yes", "No"),
          ylab="Engagement", xlab="Uses 2T2 Group for Informaiton")
t.test(engagement ~ information.source.2t2, data=dataset, var.equal=TRUE)
group_by(dataset, information.source.2t2) %>%
  summarise(
    count=n(),
    mean=mean(engagement, na.rm = TRUE),
    sd=sd(engagement, na.rm = TRUE))
cohen.d(dataset$engagement, dataset$information.source.2t2)

#--- t-test for TCM attendance and information source: Facebook ---#
ggboxplot(dataset, x="information.source.facebook", y="tcm.attendance.estimate",
          color="information.source.facebook", palette=c("#00AFBB", "#FC4E07"),
          order=c("Yes", "No"),
          ylab="TCMs Attended", xlab="Uses Facebook for Information")
t.test(tcm.attendance.estimate.log ~ information.source.facebook, data=dataset, var.equal=TRUE)
group_by(dataset, information.source.facebook) %>%
  summarise(
    count=n(),
    mean=mean(tcm.attendance.estimate, na.rm = TRUE),
    sd=sd(tcm.attendance.estimate, na.rm = TRUE))
cohen.d(dataset$tcm.attendance.estimate.log, dataset$information.source.facebook)

#--- t-test for TCM attendance and information source: Facebook 2T2 Group ---#
ggboxplot(dataset, x="information.source.2t2", y="tcm.attendance.estimate",
          color="information.source.2t2", palette=c("#00AFBB", "#FC4E07"),
          order=c("Yes", "No"),
          ylab="TCMs Attended", xlab="Uses 2T2 Group for Information")
t.test(tcm.attendance.estimate.log ~ information.source.2t2, data=dataset, var.equal=TRUE)
group_by(dataset, information.source.2t2) %>%
  summarise(
    count=n(),
    mean=mean(tcm.attendance.estimate, na.rm = TRUE),
    sd=sd(tcm.attendance.estimate, na.rm = TRUE))
cohen.d(dataset$tcm.attendance.estimate.log, dataset$information.source.2t2)

#--- t-test for clubs known and information source: Facebook ---#
ggboxplot(dataset, x="information.source.facebook", y="clubs.known.number",
          color="information.source.facebook", palette=c("#00AFBB", "#FC4E07"),
          order=c("Yes", "No"),
          ylab="Clubs Known", xlab="Uses Facebook for Information")
t.test(clubs.known.number ~ information.source.facebook, data=dataset, var.equal=TRUE)
group_by(dataset, information.source.facebook) %>%
  summarise(
    count=n(),
    mean=mean(clubs.known.number, na.rm = TRUE),
    sd=sd(clubs.known.number, na.rm = TRUE))
cohen.d(dataset$clubs.known.number, dataset$information.source.facebook)

#--- t-test for clubs known and information source: Facebook 2T2 Group ---#
ggboxplot(dataset, x="information.source.2t2", y="clubs.known.number",
          color="information.source.2t2", palette=c("#00AFBB", "#FC4E07"),
          order=c("Yes", "No"),
          ylab="Clubs Known", xlab="Uses 2T2 Group for Information")
t.test(clubs.known.number ~ information.source.2t2, data=dataset, var.equal=TRUE)
group_by(dataset, information.source.2t2) %>%
  summarise(
    count=n(),
    mean=mean(clubs.known.number, na.rm = TRUE),
    sd=sd(clubs.known.number, na.rm = TRUE))
cohen.d(dataset$clubs.known.number, dataset$information.source.2t2)

#--- t-test for clubs attended and information source: Facebook ---#
ggboxplot(dataset, x="information.source.facebook", y="clubs.attended.number",
          color="information.source.facebook", palette=c("#00AFBB", "#FC4E07"),
          order=c("Yes", "No"),
          ylab="Clubs Attended", xlab="Uses Facebook for Information")
t.test(clubs.attended.number ~ information.source.facebook, data=dataset, var.equal=TRUE)
group_by(dataset, information.source.facebook) %>%
  summarise(
    count=n(),
    mean=mean(clubs.attended.number, na.rm = TRUE),
    sd=sd(clubs.attended.number, na.rm = TRUE))
cohen.d(dataset$clubs.attended.number, dataset$information.source.facebook)

#--- t-test for clubs attended and information source: Facebook 2T2 Group ---#
ggboxplot(dataset, x="information.source.2t2", y="clubs.attended.number",
          color="information.source.2t2", palette=c("#00AFBB", "#FC4E07"),
          order=c("Yes", "No"),
          ylab="Clubs Attended", xlab="Uses 2T2 Group for Information")
t.test(clubs.attended.number ~ information.source.2t2, data=dataset, var.equal=TRUE)
group_by(dataset, information.source.2t2) %>%
  summarise(
    count=n(),
    mean=mean(clubs.attended.number, na.rm = TRUE),
    sd=sd(clubs.attended.number, na.rm = TRUE))
cohen.d(dataset$clubs.attended.number, dataset$information.source.2t2)

#--- t-test for engagement and information source: Email ---#
ggboxplot(dataset, x="information.source.email", y="engagement",
          color="information.source.email", palette=c("#00AFBB", "#FC4E07"),
          order=c("Yes", "No"),
          ylab="Engagement", xlab="Uses Email for Informaiton")
t.test(engagement ~ information.source.email, data=dataset, var.equal=TRUE)
group_by(dataset, information.source.email) %>%
  summarise(
    count=n(),
    mean=mean(engagement, na.rm = TRUE),
    sd=sd(engagement, na.rm = TRUE))

#--- t-test for TCM attendance and information source: Email ---#
ggboxplot(dataset, x="information.source.email", y="tcm.attendance.estimate",
          color="information.source.email", palette=c("#00AFBB", "#FC4E07"),
          order=c("Yes", "No"),
          ylab="TCMs Attended", xlab="Uses Email for Information")
t.test(tcm.attendance.estimate.log ~ information.source.email, data=dataset, var.equal=TRUE)
group_by(dataset, information.source.email) %>%
  summarise(
    count=n(),
    mean=mean(tcm.attendance.estimate, na.rm = TRUE),
    sd=sd(tcm.attendance.estimate, na.rm = TRUE))

#--- t-test for clubs known and information source: Email ---#
ggboxplot(dataset, x="information.source.email", y="clubs.known.number",
          color="information.source.email", palette=c("#00AFBB", "#FC4E07"),
          order=c("Yes", "No"),
          ylab="Clubs Known", xlab="Uses Email for Information")
t.test(clubs.known.number ~ information.source.email, data=dataset, var.equal=TRUE)
group_by(dataset, information.source.email) %>%
  summarise(
    count=n(),
    mean=mean(clubs.known.number, na.rm = TRUE),
    sd=sd(clubs.known.number, na.rm = TRUE))

#--- t-test for clubs attended and information source: Email ---#
ggboxplot(dataset, x="information.source.email", y="clubs.attended.number",
          color="information.source.email", palette=c("#00AFBB", "#FC4E07"),
          order=c("Yes", "No"),
          ylab="Clubs Attended", xlab="Uses Email for Information")
t.test(clubs.attended.number ~ information.source.email, data=dataset, var.equal=TRUE)
group_by(dataset, information.source.email) %>%
  summarise(
    count=n(),
    mean=mean(clubs.attended.number, na.rm = TRUE),
    sd=sd(clubs.attended.number, na.rm = TRUE))

#--- t-test for engagement and information source: Other ---#
ggboxplot(dataset, x="information.source.other", y="engagement",
          color="information.source.other", palette=c("#00AFBB", "#FC4E07"),
          order=c("Yes", "No"),
          ylab="Engagement", xlab="Uses Word of Mouth for Informaiton")
t.test(engagement ~ information.source.other, data=dataset, var.equal=TRUE)
group_by(dataset, information.source.other) %>%
  summarise(
    count=n(),
    mean=mean(engagement, na.rm = TRUE),
    sd=sd(engagement, na.rm = TRUE))

#--- t-test for TCM attendance and information source: Other ---#
ggboxplot(dataset, x="information.source.other", y="tcm.attendance.estimate",
          color="information.source.other", palette=c("#00AFBB", "#FC4E07"),
          order=c("Yes", "No"),
          ylab="TCMs Attended", xlab="Uses Word of Mouth for Information")
t.test(tcm.attendance.estimate.log ~ information.source.other, data=dataset, var.equal=TRUE)
group_by(dataset, information.source.other) %>%
  summarise(
    count=n(),
    mean=mean(tcm.attendance.estimate, na.rm = TRUE),
    sd=sd(tcm.attendance.estimate, na.rm = TRUE))

#--- t-test for clubs known and information source: Other ---#
ggboxplot(dataset, x="information.source.other", y="clubs.known.number",
          color="information.source.other", palette=c("#00AFBB", "#FC4E07"),
          order=c("Yes", "No"),
          ylab="Clubs Known", xlab="Uses Word of Mouth for Information")
t.test(clubs.known.number ~ information.source.other, data=dataset, var.equal=TRUE)
group_by(dataset, information.source.other) %>%
  summarise(
    count=n(),
    mean=mean(clubs.known.number, na.rm = TRUE),
    sd=sd(clubs.known.number, na.rm = TRUE))

#--- t-test for clubs attended and information source: Other ---#
ggboxplot(dataset, x="information.source.other", y="clubs.attended.number",
          color="information.source.other", palette=c("#00AFBB", "#FC4E07"),
          order=c("Yes", "No"),
          ylab="Clubs Attended", xlab="Uses Word of Mouth for Information")
t.test(clubs.attended.number ~ information.source.other, data=dataset, var.equal=TRUE)
group_by(dataset, information.source.other) %>%
  summarise(
    count=n(),
    mean=mean(clubs.attended.number, na.rm = TRUE),
    sd=sd(clubs.attended.number, na.rm = TRUE))

#--- Chi-square test for independence for TCM attendance reasons and residence status ---#
chisq.test(table(dataset$tcm.attendance.reasons, dataset$residence.status))
mosaicplot(table(dataset$tcm.attendance.reasons, dataset$residence.status), shade=TRUE)

#--- Chi-square test for independence for TCM attendance reasons and majority ethnicity ---#
chisq.test(table(dataset$tcm.attendance.reasons, dataset$is.white))
mosaicplot(table(dataset$tcm.attendance.reasons, dataset$is.white), shade=TRUE)

#--- Chi-square test for independence for TCM ideal time and residence status ---#
chisq.test(table(dataset$tcm.ideal.time, dataset$residence.status))
mosaicplot(table(dataset$tcm.ideal.time, dataset$residence.status), shade=TRUE)

#--- Chi-square test for independence for TCM ideal time and majority ethnicity ---#
chisq.test(table(dataset$tcm.ideal.time, dataset$is.white))
mosaicplot(table(dataset$tcm.ideal.time, dataset$is.white), shade=TRUE)

#--- Chi-square test for independence for information source: Facebook and majority ethnicity ---#
chisq.test(table(dataset$information.source.facebook, dataset$is.white))
mosaicplot(table(dataset$information.source.facebook, dataset$is.white), shade=TRUE)

#--- Chi-square test for independence for information source: Facebook and residence status ---#
chisq.test(table(dataset$information.source.facebook, dataset$residence.status))
mosaicplot(table(dataset$information.source.facebook, dataset$residence.status), shade=TRUE)

#--- Chi-square test for independence for information source: Facebook and year level ---#
chisq.test(table(dataset$information.source.facebook, dataset$year.level))
mosaicplot(table(dataset$information.source.facebook, dataset$year.level), shade=TRUE)

#--- t-test for clubs known and majority ethnicity ---#
ggboxplot(dataset, x="is.white", y="clubs.known.number",
          color="is.white", palette=c("#00AFBB", "#FC4E07"),
          order=c("Yes", "No"),
          ylab="Clubs Known", xlab="Is Majority Ethnicity")
t.test(clubs.known.number ~ is.white, data=dataset, var.equal=TRUE)
group_by(dataset, is.white) %>%
  summarise(
    count=n(),
    mean=mean(clubs.known.number, na.rm = TRUE),
    sd=sd(clubs.known.number, na.rm = TRUE))
cohen.d(dataset$clubs.known.number, dataset$is.white)

#--- t-test for clubs known and majority ethnicity ---#
ggboxplot(dataset, x="is.white", y="clubs.attended.number",
          color="clubs.attended.number", palette=c("#00AFBB", "#FC4E07"),
          order=c("Yes", "No"),
          ylab="Clubs Attended", xlab="Is Majority Ethnicity")
t.test(clubs.attended.number ~ is.white, data=dataset, var.equal=TRUE)
group_by(dataset, is.white) %>%
  summarise(
    count=n(),
    mean=mean(clubs.attended.number, na.rm = TRUE),
    sd=sd(clubs.attended.number, na.rm = TRUE))

#--- t-test for clubs known and residence status ---#
ggboxplot(dataset, x="residence.status", y="clubs.known.number",
          color="residence.status", palette=c("#00AFBB", "#FC4E07"),
          order=c("Resident", "Non-Resident"),
          ylab="Clubs Known", xlab="Residence Status")
t.test(clubs.known.number ~ residence.status, data=dataset, var.equal=TRUE)
group_by(dataset, residence.status) %>%
  summarise(
    count=n(),
    mean=mean(clubs.known.number, na.rm = TRUE),
    sd=sd(clubs.known.number, na.rm = TRUE))
cohen.d(dataset$clubs.known.number, dataset$residence.status)

#--- t-test for clubs attended and residence status ---#
ggboxplot(dataset, x="residence.status", y="clubs.attended.number",
          color="residence.status", palette=c("#00AFBB", "#FC4E07"),
          order=c("Resident", "Non-Resident"),
          ylab="Clubs Attened", xlab="Residence Status")
t.test(clubs.attended.number ~ residence.status, data=dataset, var.equal=TRUE)
group_by(dataset, residence.status) %>%
  summarise(
    count=n(),
    mean=mean(clubs.attended.number, na.rm = TRUE),
    sd=sd(clubs.attended.number, na.rm = TRUE))
cohen.d(dataset$clubs.attended.number, dataset$residence.status)

#### Export Data for Publication ####

#--- Read data again ---#
open_dataset<-read.csv("Clubs & Governance Survey 2019.csv")
View(open_dataset)

#--- Randomize row order---#
open_dataset<-open_dataset[sample(nrow(open_dataset)),]

#--- Remove ancillary rows---#
open_dataset$timestamp<-NULL
open_dataset$tcm.attendance.comments<-NULL
open_dataset$tcm.engagement.comments<-NULL

#--- Remove non-significant identifying information ---#
open_dataset$social.year<-NULL
open_dataset$study<-NULL
open_dataset$international.status<-NULL
open_dataset$gender<-NULL

#--- Create comma separated values file ---#
write.csv(open_dataset, file="Open Data TCM Clubs & Governance Survey 2019.csv")