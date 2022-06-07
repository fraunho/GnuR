# Kruskal-Wallis test
#   
# Kruskal-Wallis test by rank is a non-parametric alternative to one-way ANOVA test, 
# which extends the two-samples Wilcoxon test in the situation where there are more than two groups. 
# It’s recommended when the assumptions of one-way ANOVA test are not met. 
# This tutorial describes how to compute Kruskal-Wallis test in R software.
# 
# 
library(readxl)
LAMP1_Recruitment <- read_excel("D:/Martin/Desktop/Stats/Fig2E-LAMP-comb.xlsx")
View(LAMP1_Recruitment)
#my_data_o <- PlantGrowth
my_data <- LAMP1_Recruitment
# 
# Check your data
# 
# # print the head of the file
head(my_data)
# 
# 
# In R terminology, the column “group” is called factor and the different categories 
# (“ctr”, “trt1”, “trt2”) are named factor levels. The levels are ordered alphabetically.
# 
# # Show the group levels
levels(my_data$group)
# 

# 
# If the levels are not automatically in the correct order, re-order them as follow:
#   
my_data$group <- ordered(my_data$group,
                        levels = c("Ctrl", "ionomycin", "alpha_Toxin", "Sa WT SNT", "Sa delta_hla", "Sa delta_hlb"))
# 
# It’s possible to compute summary statistics by groups. The dplyr package can be used.
# 
# Compute summary statistics by groups:
#   
library(dplyr)
group_by(my_data, group) %>%
  summarise(
    count = n(),
    mean = mean(EventsPerCell, na.rm = TRUE),
    sd = sd(EventsPerCell, na.rm = TRUE),
    median = median(EventsPerCell, na.rm = TRUE),
    IQR = IQR(EventsPerCell, na.rm = TRUE)
  )
# 

# 
# Visualize the data using box plots
# 
# To use R base graphs read this: R base graphs. Here, we’ll use the ggpubr R package 
# for an easy ggplot2-based data visualization.
# 


# 
# Visualize your data with ggpubr:
#   
# Box plots
# ++++++++++++++++++++
# Plot EventsPerCell by group and color by group
library("ggpubr")
ggboxplot(my_data, x = "group", y = "EventsPerCell",
          color = "group", palette = c("#00AFBB", "#E7B800", "#FC4E07", "#00ff00", "#ff0000", "#0000ff"),
          order = c("Ctrl", "ionomycin", "alpha_Toxin", "Sa WT SNT", "Sa delta_hla", "Sa delta_hlb"),
          ylab = "EventsPerCell", xlab = "Treatment")



# 
# # Mean plots
# # ++++++++++++++++++++
# # Plot EventsPerCell by group
# # Add error bars: mean_se
# # (other values include: mean_sd, mean_ci, median_iqr, ....)


library("ggpubr")
ggline(my_data, x = "group", y = "EventsPerCell",
       add = c("mean_se", "jitter"),
       order = c("Ctrl", "ionomycin", "alpha_Toxin", "Sa WT SNT", "Sa delta_hla", "Sa delta_hlb"),
       ylab = "EventsPerCell", xlab = "Treatment")


# 
# Compute Kruskal-Wallis test
# 
# We want to know if there is any significant difference between the average EventsPerCells 
# of plants in the 3 experimental conditions.
# 
# The test can be performed using the function kruskal.test() as follow:
#   
kruskal.test(EventsPerCell ~ group, data = my_data)


# 
# 
# Kruskal-Wallis rank sum test
# data:  EventsPerCell by group
# Kruskal-Wallis chi-squared = 7.9882, df = 2, p-value = 0.01842
# 
# Interpretation 
# As the p-value is less than the significance level 0.05, we can conclude that there are 
# significant differences between the treatment groups.

# From the output of the Kruskal-Wallis test, we know that there is a significant difference 
# between groups, but we don’t know which pairs of groups are different.


#################
# Multiple pairwise-comparison between groups
# 
# 
# It’s possible to use the function pairwise.wilcox.test() to calculate pairwise comparisons 
# between group levels with corrections for multiple testing.
# 
pairwise.wilcox.test(my_data$EventsPerCell, my_data$group,
                   p.adjust.method = "BH")
# 
# 
# Pairwise comparisons using Wilcoxon rank sum test 
# data:  PlantGrowth$EventsPerCell and PlantGrowth$group 
# ctrl  trt1 
# trt1 0.199 -    
#   trt2 0.095 0.027
# P value adjustment method: BH 

# 
# The pairwise comparison shows that, only trt1 and trt2 are significantly different (p < 0.05).
