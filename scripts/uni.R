# load packages 
library(tidyverse)
install.packages("easystats")
install.packages("broom.helpers")
library(broom.helpers)
library(cardx)
library(easystats)
library(gtsummary)
library(gt)

# load data 
data <- readxl::read_excel("clean_data/AMR_Clean.xlsx")


# Factors associated with knowledge score in the linear regression analysis
  data |> 
    select(1:9, knowledge_score) |> 
     tbl_uvregression(
        method = lm, 
         y = knowledge_score
       ) |> 
       bold_p(t = 0.05) |> 
       as_gt() |> 
      gtsave("tables/UV_LinReg.docx")

  # Factors associated with attitude score in the linear regression analysis
  data |> 
    select(1:9, attitude_score) |> 
    tbl_uvregression(
      method = lm, 
      y = attitude_score
    ) |> 
    bold_p(t = 0.05) |> 
    as_gt() |> 
    gtsave("tables/UV_LinReg1.docx")
  
  # Factors associated with practice score in the linear regression analysis
  
  data |> 
    select(1:9, practice_score) |> 
    tbl_uvregression(
      method = lm, 
      y = practice_score
    ) |> 
    bold_p(t = 0.05) |> 
    as_gt() |> 
    gtsave("tables/UV_LinReg2.docx")
  
  