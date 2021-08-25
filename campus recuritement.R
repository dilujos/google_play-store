
# Introduction
# we are looking into the campus recruitment dataset.
# 
# First off, what exactly is a job placement?
#   
#   A job placement is very similar to an internship, just much longer. Doing placement as part of the course provides huge benefit by providing great working experience and increase your employability when you are ready to enter the market. No, you won't be just making coffee although you will be responsible for quite a fair amount of general administrative duties. Since you are considered an employee in the company. you will have the opportunity to develop your skills through meatier assignments. After you have completed your job placement, you may be given the opportunity to join the company if you exceed their expectations!
# 
# For this dataset, we have created the following objectives to answer common questions frequently asked.
# 
# Objectives
# Which factor influenced a candidate in getting placed?
# Does percentage matters for one to get placed?
# Which degree specialization is much demanded by corporate?
# Play with the data conducting all statistical tests.




# This data set consists of Placement data of students in our campus. 
#It includes secondary and higher secondary school percentage and specialization. 
#It also includes degree specialization, type and Work experience and salary offers to the placed students.
# Columns:
#   
#   (decimal #) sl_no: Serial Number
#    
#    (string A) genderGender: Male='M', Female='F'
#    
#    (decimal #)ssc_p : Secondary: Education percentage- 10th Grade
#     
#     (string A) ssc_b:Board of Education- Central/ Others
#     
#     (decimal #) hsc_pHigher: Secondary Education percentage- 12th Grade
#       
#       (string A) hsc_b: Board of Education- Central/ Others
#       
#       (string A) hsc_s: Specialization in Higher Secondary Education
#       
#       (decimal #) degree_p: Degree Percentage
#         
#         (string A) degree_tUnder: Graduation(Degree type)- Field of degree education
#         
#         (Boolean T/F) workex : Work Experience
#         
#         (decimal #) etest_p: Employability test percentage ( conducted by college)
#           
#           (string A) specialisation: Post Graduation(MBA)- Specialization
#           
#           (decimal #) mba_p: MBA percentage
#             
#             (string A) status: Status of placement- Placed/Not placed
#             
#             (decimal #) salary: Salary offered by corporate to candidates





#2.0 Acquiring and Clean Data
#2.1 Install/Load Packages and Import Data


library(reshape2)
library(tidyr)
library(dplyr)
library(ggplot2)

#1.read the data
df=read.csv("campus recuritement.csv")

#2.first 10 data
head(df,10)

#3.last 10 data
tail(df,10)

#3. shape of data
dim(df)

#4.print all col names
colnames(df)

#5.datatype of the columns
str(df)

#6. print information and summary
summary(df)

#7 find null values
sum(is.na(df))

# there are a total of 162 null values in salary coloums

#Replace NAs in Salary column
df[is.na(df)] <- 0 
summary(df)

#8Check for duplication
duplicated(df)

# 3.0 Data Visualisation
# 3.1 Boxplot



# An interesting topic that will always captured people's attention, is the difference in salary between the gender. And there goes my first propose question for my visaulisation analysis -
#What is the difference in salary between the male and female. And the best visualisation to aid my analysis is by using the Boxplot diagram.
# 
# A boxplot is a standardized way of displaying the distribution of data based on 
#a five number summary ("minimum", first quartile (Q1), median, third quartile (Q3), and "maximum"). 
#It can tell you about your outliers and what their values are. It can also tell you if your data is symmetrical, how tightly your data is grouped, and if and how your data is skewed. reference:https://towardsdatascience.com/understanding-boxplots-5e2df7bcbd51
# 
# Firstly,i have filter salary that are > 0 as those are student that have no placement thus,
#having 0 as their salary's indication. Then, options(scipen = 999) helps me to replace the scientific notation to values in the bloxplot diagram. Finally, utilize ggplot to create the following charts.
# 
# The gender ratio in the dataset is about 1 (female): 2(male)
# 
# The boxplot diagram shows that more male student are having high salary -
#above third quater and dipicts many outliers situation. Salary are from 400 - 800k. Whereas, there is only one female student with 650k salary.
# 
# The similaries of in the gender were that they have bigger portion of student have salary between median - third quater, 
#than first quater - median. And female student has lower median as compare to male student.
# 

#9
  dataVis <- filter(df, salary != 0) #filter out 0 as data with 0 values have no placement yet
  
  options(scipen=999)
  #Can gender affects salary?
  ggplot(dataVis, aes(salary, gender)) + geom_boxplot()
  #Finding median for female since we c
  
  
  
#library(scales)
  ggplot(dataVis, aes(gender)) + geom_bar()

  
# 3.2 Bar Chart
# A quick overview of all the categorical data Status - student with and w/o placement WorkExp -
# student with and w/o work experience DegreeType - distribution of student degree type MbaSpecialization -
# distribution of student mba specialization SecBoardOfEducation - 
# no. belong to central/others HigherSecBoardofEducation - no. belong to central/others 
#10
  ggplot(df, aes(status)) + geom_bar(fill = "red", color = "green")
  
#11
  ggplot(df, aes(workex)) + geom_bar(fill = "orange", color = "white")
#12
  ggplot(df, aes(degree_t)) + geom_bar(fill = "yellow", color = "white")
#13
  ggplot(df, aes(specialisation)) + geom_bar(fill = "green", color = "white")
#14
  ggplot(df, aes(ssc_b)) + geom_bar(fill = "blue", color = "white")
#15
  ggplot(df, aes(hsc_s)) + geom_bar(fill = "purple", color = "white")
  
  
# 3.2 Stacked and 2 Dimensions Bar Chart
# A quick overview of the categorical data vs status.
  
#16
  ggplot(df, aes(specialisation, fill = status)) + geom_bar() +facet_wrap(~status)
  ggplot(df, aes(specialisation, fill = status)) + geom_bar() 

#17
  ggplot(df, aes(degree_t, fill = status)) + geom_bar() + facet_wrap(~status)
  
#18
  ggplot(df, aes(workex, fill = status)) + geom_bar() + facet_grid(rows = vars(status))
  
ggplot(df, aes(ssc_b, fill = status)) + geom_bar(position = "dodge")  
ggplot(df, aes(hsc_s, fill = status)) + geom_bar(position = "dodge")


# ScatterPlot
# Using ScatterPlot to visualise the relationship between per degree % and salary.
# 
# Apparently there is no trends between the 2 variables as most 
# of the fresh grad are getting the same salary regardless of their degree %.  

ggplot(dataVis, aes(degree_p, salary, color = degree_t, size = salary)) + geom_jitter(width = 80, shape = 17, size = 3, alpha = 0.6) + theme_classic() + 
  labs(title = "Degree Percentage vs Salary", subtitle = "high percentage = higher salary?") +
  xlab("Degree %") + ylab("Salary(USD")



ggplot(dataVis, aes(mba_p, salary, color = specialisation, size = salary)) +         geom_jitter(width = 80, shape = 17, size = 3, alpha = 0.6) + theme_dark() + 
  labs(title = "MBA Percentage vs Salary", subtitle = "high percentage = higher salary?") +
  xlab("MBA %") + ylab("Salary(USD)")  



ggplot(dataVis, aes(gender, salary, fill =specialisation)) + geom_col() +
  theme(axis.text.x = element_text(face="bold", color="#993333", 
                                   size=14, angle=45),
        axis.text.y = element_text(face="bold", color="#993333", 
                                   size=14, angle=45)) +
  scale_y_continuous(breaks = seq(0,30000000, 5000000))



# ggplot(df, aes(degree_p, fill = gender)) + geom_histogram(binwidth = 5) +
#   xlab("Percent %") +
#   ylab("Frequency") +
#   scale_y_continuous(limits = c(0,70), breaks = seq(0,70,10))

