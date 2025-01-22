
# =================================================== #
# Assignment 1
# Author:Rasel sarker
# Email: rasel.sarker6933@gmail.com
# Date of submission:22 January 2025
# =================================================== #


# Task 1 ------------------------------------------------------------------
# Read the data from the folder using suitable function into a variable named "df1"
# You will find details of the variables in the excel sheet named "Data Dictionary"
library(readxl)
library(dplyr)
df1<- read_excel("Data\\StudentSurveyData.xlsx")
print(df1)


# Task 2 ------------------------------------------------------------------
# Check structure of the data

str(df1)

# Task 3 ------------------------------------------------------------------
# See summary of the data frame
summary(df1)


# Task 4 ------------------------------------------------------------------
# (1) Use the function unique() to see the unique categories in the Major column of the data frame.

unique_categ<- unique(df1[,c("Major")])
print(unique_categ)

# (2) Use the function table() to see the frequency of each category in Major column in the data frame.
fre_major_catge<- table(df1[,c("Major")])
print(fre_major_catge)


# Task 5 ------------------------------------------------------------------
# Create a new data frame named df2, where the "IS" category in Major column is replaced by "Insormation Systems"
df2<- df1
print(df2)
df2[df2=='IS'] <- "Information System"
print(df2)

# Check frequency of categories in Major column again.
fre_marjor= table(df2[,c('Major')])
print(fre_marjor)

# Task 5 ------------------------------------------------------------------
# Run the following code ONLY ONCE (DO NOT CHANGE ANYTHING).
set.seed(2025)
df2$Spending[sample(nrow(df2), nrow(df2) * 0.05)] <- NA  # introduces 5% missing values
df2$`Text Messages`[sample(nrow(df2), nrow(df2) * 0.1)] <- NA  # introduces 10% missing values

# Create a function called 'data_na_count' with one argument (data) that returns 
# a vector with number of missing values in each columns
data_na_count<- function(data){
  if (!is.data.frame(data)){
    stop("Input must be a data frame")
  }
  na_count <- sapply(data, function(x) sum(is.na(x)))
  
  return(na_count)
}

missing_values<- data_na_count(df2)
print(missing_values)




# Task 7 ------------------------------------------------------------------
# Create a new data frame named df_male that contains data for male students only.

df_male <- df1[df1$Gender == "Male", ]

print(df_male)


# How many missing values are there in the Spending column of the df_male data frame?
# Your answer: 

missing_spending <- sum(is.na(df_male$Spending))

print(missing_spending)


# Task 8 ------------------------------------------------------------------
# From the df2 data frame find the top 3 students having highest GPA who are 
# unemployed and are currently in 'Senior' class

df_senior_unemployed <- df2[df2$Employment == "Unemployed" & df2$Class == "Senior", ]

df_senior_unemployed <- df_senior_unemployed[order(-df_senior_unemployed$GPA), ]

top_3_students <- head(df_senior_unemployed, 3)

print(top_3_students)

# Task 9 ------------------------------------------------------------------
# In the df2 data frame, change data type of all the character columns to factor columns
# Do not create a new data frame
# Check the data type of the columns by inspecting the data frame using str() 

df2[] <- lapply(df2, function(x) if(is.character(x)) as.factor(x) else x)

str(df2)


# See the summary of the new data frame
summary(df2)

# Task 10 -----------------------------------------------------------------
# Select only those students who use Laptop for study. Export this data into an Excel file.
# Install and load the openxlsx package
#install.packages("openxlsx")
library(openxlsx)

df_laptop <- df2[df2$Computer == "Laptop", ]

# Export the filtered data to an Excel file
write.xlsx(df_laptop, file = "df_laptop.xlsx")

print("Data has been exported to df_laptop.xlsx")

