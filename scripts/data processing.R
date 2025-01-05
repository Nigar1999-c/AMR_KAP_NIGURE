library(tidyverse)
library(readxl)

data <- read_excel("raw_data/AMR_KAP_Data.xlsx")


sum(is.na(data))
data <- na.omit(data)
sum(is.na(data))


AMR <- data |> 
  select(12:39)
colnames(AMR) <- paste0("Q", 1:28)

# 1.Knowledge of antibotic
antibiotic_knowledge <- AMR |> 
  select(Q1:Q12) |> 
  mutate(across(Q1:Q3, ~case_when(
    . == "Yes" ~ 1, 
    . == "No" ~ 0, 
    . == "Don't know" ~ 0, 
    TRUE ~ NA_real_
  ))) |> 
  mutate(across(Q4:Q5, ~case_when(
    . == "No" ~ 1, 
    . == "Yes" ~ 0, 
    . == "Don't know" ~ 0, 
    TRUE ~ NA_real_
  ))) |> 
  mutate(Q6 = case_when(
    Q6 == "Yes" ~ 1,
    Q6 == "No" ~ 0, 
    Q6 == "Don't know" ~ 0,
    TRUE ~ NA_real_
  )) |> 
  mutate(Q7 = case_when(
    Q7 == "No" ~ 1,
    Q7 == "Yes" ~ 0, 
    Q7 == "Don't know" ~ 0,
    TRUE ~ NA_real_
  )) |> 
  mutate(across(Q8:Q12, ~case_when(
    . == "Yes" ~ 1, 
    . == "No" ~ 0, 
    . == "Don't know" ~ 0, 
    TRUE ~ NA_real_
  ))) |> 
  # Calculate row-wise mean for knowledge score
  rowwise() |> 
  mutate(knowledge_score = mean(c_across(Q1:Q12) * 100, na.rm = TRUE)) |> 
  ungroup() |> 
  # Grading a person's knowledge level
  mutate(antibiotic_knowledge = case_when(
    knowledge_score <= 49 ~ "Poor", 
    knowledge_score > 49 & knowledge_score < 80 ~ "Moderate",
    knowledge_score >= 80 ~ "Good",
    TRUE ~ NA_character_
  ))




# 2. knowledge of attitude 

attitude_knowledge <- AMR |> 
  select(Q13:Q22) |> 
  mutate(across(Q13:Q22, ~case_when(
    . == "Disagree" ~ 1,
    . == "Agree" ~ 0, 
    . == "Neutral" ~ 0, 
    TRUE ~ NA_real_
  ))) |> 
  rowwise() |> 
  mutate(attitude_score = mean(c_across(Q13:Q22) * 100, na.rm = TRUE)) |> 
  ungroup() |> 
  #Grading a person's attitude level
  mutate(attitude_knowledge = case_when(
    attitude_score <= 49 ~ "Negative", 
    attitude_score > 49 & attitude_score < 80 ~ "Uncertain",
    attitude_score >= 80 ~ "Positive",
    TRUE ~ NA_character_
  ))



#knowledge of practice

practice_knowledge <- AMR |> 
  select(Q23:Q28) |> 
  mutate(
    Q23 = case_when(
      Q23 == "No" ~ 1,
      Q23 == "Yes" ~ 0,
      TRUE ~ NA_real_
    ),
    Q28 = case_when(
      Q28 == "Yes" ~ 1,
      Q28 == "No" ~ 0,
      TRUE ~ NA_real_
    )
  ) |> 
  mutate(across(Q24:Q25, ~case_when(
    . == "Yes" ~ 1, 
    . == "No" ~ 0, 
    TRUE ~ NA_real_
  ))) |> 
  mutate(across(Q26:Q27, ~case_when(
    . == "No" ~ 1, 
    . == "Yes" ~ 0, 
    TRUE ~ NA_real_
  ))) |> 
  # Calculate row-wise mean for practice score
  rowwise() |> 
  mutate(practice_score = mean(c_across(Q23:Q28) * 100, na.rm = TRUE)) |> 
  ungroup() |> 
  # Grading a person's practice score
  mutate(practice_knowledge = case_when(
    practice_score <= 79 ~ "Misuse", 
    practice_score >= 80 ~ "GOOD",
    TRUE ~ NA_character_
  ))

# combined the data 


demographics <- data |> 
    select(1:11)


information_source <- data |> 
     select(41:49)

#combine all sections
clean_data <- cbind(demographics,antibiotic_knowledge, attitude_knowledge, 
                    practice_knowledge,information_source)

if (!dir.exists("clean_data")) {
  dir.create("clean_data")
}


# export the data as CSV
write.csv(clean_data, "clean_data/AMR_Clean.csv", row.names = FALSE)

install.packages("openxlsx")

library(openxlsx)

# Ensure the directory exists
if (!dir.exists("clean_data")) {
  dir.create("clean_data")
}



# Export the data as an Excel file
write.xlsx(clean_data, "clean_data/AMR_Clean.xlsx", rowNames = FALSE)
