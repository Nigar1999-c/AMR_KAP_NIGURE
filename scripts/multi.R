# load packages 
library(tidyverse)
library(easystats)
library(gtsummary)
library(gt)
library(readxl)
library(dplyr)


# load data 
data <- readxl::read_excel("clean_data/AMR_Clean.xlsx")


# recode knowledge level
data <- data |> 
  mutate(antibiotic_knowledge = case_when(
    knowledge_score <= 50 ~ 0,
    knowledge_score >50 ~ 1
  )) |> mutate(across(antibiotic_knowledge, as.factor))

# recode attitude level
data <- data |> 
  mutate(attitude_knowledge = case_when(
    attitude_score <= 50 ~ 0,
    attitude_score >=51 ~ 1
  ))|> mutate(across(attitude_knowledge, as.factor))

# recode practice level
data <- data |> 
  mutate(practice_knowledge = case_when(
    practice_score <= 50 ~ 0,
    practice_score >=51 ~ 1
  ))|> mutate(across(practice_knowledge, as.factor))


# Table 4. Factors associated with the level of knowledge among parents of school-going children


# multivariate logistic regression
kn_mv_logreg <- glm(antibiotic_knowledge ~ `Parent’s age (years)`+ `Parent’s sex` +
                      `Parent’s education level`+ `Employment status` + `Family type` +
                      `Your average household income per month (BDT)` + `Child’s sex`
                    + `Child’s age (years)` + `Number of children`,
                    data = data, family = "binomial")
report(kn_mv_logreg)

# multivariate logistic regression but in tabular form
kn_mv_logreg |> 
  tbl_regression(exponentiate = TRUE) |>
  bold_p(t = 0.05) |> 
  as_gt() |> 
  gtsave("tables/Table5.docx")


## Table 5. Factors associated with the level of attitudes towards antibiotic resistance among parents of schoolgoing children 

# multivariate logistic regression
at_mv_logreg <- glm(attitude_knowledge ~ `Parent’s age (years)`+ `Parent’s sex` +
                      `Parent’s education level`+ `Employment status` + `Family type` +
                      `Your average household income per month (BDT)` + `Child’s sex`
                    + `Child’s age (years)` + `Number of children`,
                    data = data, family = "binomial")
report(at_mv_logreg)

# multivariate logistic regression but in tabular form
at_mv_logreg |> 
  tbl_regression(exponentiate = TRUE) |>
  bold_p(t = 0.05) |> 
  as_gt() |> 
  gtsave("tables/Table6.docx")



# Table 6. Factors associated with the level of practices regarding antibiotic resistance among parents of schoolgoing children

# multivariate logistic regression
pr_mv_logreg <- glm(practice_knowledge ~ `Parent’s age (years)`+ `Parent’s sex` +
                      `Parent’s education level`+ `Employment status` + `Family type` +
                      `Your average household income per month (BDT)` + `Child’s sex`
                    + `Child’s age (years)` + `Number of children`,
                    data = data, family = "binomial")
report(pr_mv_logreg)


# multivariate logistic regression but in tabular form
pr_mv_logreg |> 
  tbl_regression(exponentiate = TRUE) |>
  bold_p(t = 0.05) |> 
  as_gt() |> 
  gtsave("tables/Table7.docx")





