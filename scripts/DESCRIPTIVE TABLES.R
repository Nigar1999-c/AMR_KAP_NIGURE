#load packageS
install.packages("gtsummary")
install.packages("gt")
library(tidyverse)
library(gtsummary)
library(gt)

#load clean data
data <- readxl::read_excel("clean_data/AMR_Clean.xlsx")

#table 1:Demographic characteristics of study participants
data |> 
  select(1:11) |> 
  tbl_summary() |> 
  as_gt() |> 
  gtsave("tables/table1.docx")

#table 2:Major sources of information about antibiotic parents

data |> 
  select(46:50) |> 
  tbl_summary() |> 
  as_gt() |> 
  gtsave("tables/table4.docx")

#table 3:Level of knowledge, attitudes, and practices regarding antibiotic resistance

data |> 
  select(25,37,45) |> 
  tbl_summary() |> 
  as_gt() |> 
  gtsave("tables/table2.docx")


