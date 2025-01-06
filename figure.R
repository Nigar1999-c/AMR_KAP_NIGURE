#install packages
install.packages(c("ggthemes", "hrbrthemes", "ggsci", "ggpubr", "RcolorBrewer"))
install.packages(c("ISLR","likert"))
install.packages("sjPlot")
install.packages("ggplot2")


# load packages
library(tidyverse)
library(ggthemes)
library(ggsci)
library(ggpubr)
library(RColorBrewer)
library(ISLR)
library(sjPlot)
library(likert)
library(ggplot2)
## data import
data2 <- read.csv("clean_data/AMR_Clean.csv")

## Check data structure
glimpse(data)

#Distribution of knowledge of antibiotic resistance among parents of school-going children (N=704)
knowledege <- c(rep("Antibiotic kills the bacteria(Yes)", 3),
                rep("Amoxicillin is an antibiotic(Yes)", 3), 
                rep("Azithromycin is an antibiotic(Yes)", 3),
                rep("Paracetamol is an antibiotic(No)", 3),
                rep("Antibiotic kills the virus(No)", 3),
                rep("Antibiotics used to treat diarrhoea(Yes)", 3),
                rep("Antibiotics are useful for flu and cough(No)", 3),
                rep("Antibiotic resistant bacteria are difficult to treat(Yes)", 3),
                rep("Misuse of antibiotics can lead to antibiotic resistant bacteria(Yes)", 3),
                rep("Antibiotics can kill normal flora(Yes)", 3),
                rep("Antibiotics can cause allergic reactions(Yes)", 3),
                rep("Infectious disease are becoming difficult to treat with antibiotics(Yes)", 3))

Response <- rep(c("Don't know", "No", "Yes"), 12)
Percentage <- c(38, 6, 56, 63, 11, 26, 56, 8, 36, 12, 79, 9, 44, 12, 44, 14, 52, 34, 5, 39, 57, 42, 11, 47, 15, 9, 75, 20, 38, 42, 45, 13, 41, 39, 27, 34)

knowledege_bar <- data.frame(knowledege, Response, Percentage)



# Customize the bar plot
ggplot(knowledege_bar, aes(fill = Response, x = Percentage, y = knowledege)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = c("#FFA07A", "#C0C0C0", "#4682B4")) +  # Custom colors
  labs(
    title = "Distribution of Knowledge About Antibiotic Resistance",
    subtitle = "Among Parents of School-Going Children (N=704)",
    x = "Percentage (%)",
    y = "Knowledge Categories",
    fill = "Response"
  ) +
  theme_pubr(base_size = 10, base_family = "Arial") +
  theme(
    axis.text.y = element_text(size = 8),
    axis.text.x = element_text(size = 10),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "bottom"
  )

# Save the plot with high resolution
ggsave("figure1.png", dpi = 300, width = 10, height = 6)


# Attitude among parents of school-going children (N=704)
Attitude <- c(rep("I will see another doctor if the first one has not been prescribed antibiotics(Disagree)", 3),
              rep("I am not satisfied if the doctor does not prescribe an antibiotic to me(Disagree)", 3),
              rep("Antibiotics are safe and hence can be used commonly(Disagree)", 3),
              rep("Sick child is given antibiotics, even there is no indication(Disagree)", 3),
              rep("Antibiotics can improve fever in children(Disagree)", 3),
              rep("A child with cold is given antibiotics(Disagree)",3),
              rep(" I stop antibiotics when my child condition improves(Disagree)",3),
              rep(" I reusing the same antibiotics for similar symptoms(Disagree)", 3),
              rep(" Leftover antibiotics are good to keep at home in case I might need them for my child later on(Disagree)", 3),
              rep(" Doctors often take time to inform parents how antibiotics should be used for their children(Disagree)", 3))

                

Response <- rep(c("Agree", "Disagree", "Neutral"), 10)
Percentage <- c(16, 81, 3, 16, 80, 4, 28, 64, 9, 20, 75, 6, 64, 31, 6, 62, 33, 5, 26, 74, 0, 27, 71, 1, 15, 84, 1, 52, 42, 5)

Attitude_bar <- data.frame(Attitude, Response, Percentage)

# Customize the bar plot for Attitude data
ggplot(Attitude_bar, aes(fill = Response, x = Percentage, y = Attitude)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(
    aes(label = paste0(Percentage, "%")),           # Add percentage labels with "%" sign
    position = position_stack(vjust = 0.5),        # Place labels in the middle of the segments
    size = 3,                                      # Adjust text size
    color = "black"                                # Text color
  ) +
  scale_fill_manual(values = c("#FFA07A", "#C0C0C0", "#4682B4")) +  # Custom colors
  labs(
    title = "Distribution of Attitude About Antibiotic Usage",
    subtitle = "Among Parents of School-Going Children (N=704)",
    x = "Percentage (%)",
    y = "Attitude Categories",
    fill = "Response"
  ) +
  theme_pubr(base_size = 10, base_family = "Arial") +
  theme(
    axis.text.y = element_text(size = 8),
    axis.text.x = element_text(size = 10),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "bottom"
  )

# Save the plot with high resolution
ggsave("figure2.png", dpi = 300, width = 10, height = 6)



#practice among parents of school-going children (N=704)
Practice <- c(rep("I give my children antibiotics(No)", 2),
               rep("I check expiring date of antibiotic before giving to children(Yes)", 2),
                rep("I seek medical advice before giving antibiotic to my children(Yes)", 2),
                rep("I give my children antibiotics when they get cough(No)", 2),
                rep("I like to take antibiotic from pharmacy instead of taking from doctor(No)", 2),
                rep("My child should complete a given dose, even he improve after 2 dose(Yes)", 2))
                

Response <- rep(c("No", "Yes"), 6)
Percentage <- c(31, 69, 18, 82, 64, 36, 42, 58, 49, 51, 24, 76)

Practice_bar <- data.frame(Practice, Response, Percentage)


# Create the bar plot for Practice data
ggplot(Practice_bar, aes(fill = Response, x = Percentage, y = Practice)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(
    aes(label = paste0(Percentage, "%")),           # Add percentage labels with "%" sign
    position = position_stack(vjust = 0.5),        # Place labels in the middle of the segments
    size = 3,                                      # Adjust text size
    color = "black"                                # Text color
  ) +
  scale_fill_manual(values = c("#FF6347", "#4682B4")) +  # Colors: Tomato Red for "No", Steel Blue for "Yes"
  labs(
    title = "Practice Among Parents of School-Going Children",
    subtitle = "Distribution of Responses (N=704)",
    x = "Percentage (%)",
    y = "Practice Categories",
    fill = "Response"
  ) +
  theme_pubr(base_size = 10, base_family = "Arial") +
  theme(
    axis.text.y = element_text(size = 8),
    axis.text.x = element_text(size = 10),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "bottom"
  )

# Save the plot with high resolution
ggsave("figure_practice.png", dpi = 300, width = 10, height = 6)









