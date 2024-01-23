#Moses Kinoti Kariithi - Shamiri R Programming Exercise
#importing data
shamiri_stats <-read.csv(file.choose())
shamiri_stats

#---CONDUCTING EXPLORATORY DATA ANALYSIS

nrow(shamiri_stats) #658 rows
str(shamiri_stats) #to observe structure of the data
shamiri_stats[1:5,] #subsetting to observe the first 5 rows in the dataset

#calculating mean and mode results for the PHQ items
#using the summarise function of the dplyr package
library("dplyr")
shamiri_phq_mean <- summarise(shamiri_stats, phq1_mean = mean(PHQ1), phq2_mean = mean(PHQ2),
                              phq3_mean = mean(PHQ3), phq4_mean = mean(PHQ4), phq5_mean = mean(PHQ5),
                              phq6_mean = mean(PHQ6), phq7_mean = mean(PHQ7), phq8_mean = mean(PHQ8))

print(shamiri_phq_mean, row.names = FALSE)

#calculating mode (most common) results for the PHQ items
#using custom defined function
get_mode <- function(x) {
  u <- unique(x)
  tab <- tabulate(match(x,u))
  u[tab == max(tab)]
}

shamiri_phq_mode <- summarise(shamiri_stats, phq1_mode = get_mode(PHQ1), phq2_mode = get_mode(PHQ2),
                              phq3_mode = get_mode(PHQ3), phq4_mode = get_mode(PHQ4), phq5_mode = get_mode(PHQ5),
                              phq6_mode = get_mode(PHQ6), phq7_mode = get_mode(PHQ7), phq8_mode = get_mode(PHQ8))

print(shamiri_phq_mode, row.names = FALSE)

#calculating the mean and mode results for GAD items
#using summarise function from the dplyr package
shamiri_gad_mean <- summarise(shamiri_stats, gad1_mean = mean(GAD1), gad2_mean = mean(GAD2),
                              gad3_mean = mean(GAD3), gad4_mean = mean(GAD4), gad5_mean = mean(GAD5),
                              gad6_mean = mean(GAD6), gad7_mean = mean(GAD7))

print(shamiri_gad_mean, row.names = FALSE)

shamiri_gad_mode <- summarise(shamiri_stats, gad1_mode = get_mode(GAD1), gad2_mode = get_mode(GAD2),
                              gad3_mode = get_mode(GAD3), gad4_mode = get_mode(GAD4), gad5_mode = get_mode(GAD5),
                              gad6_mode = get_mode(GAD6), gad7_mode = get_mode(GAD7))

print(shamiri_gad_mode, row.names = FALSE)

#Grouping mean of PHQ1 responses by age, tribe, gender and school
#using dplyr group_by() to group on multiple columns and get the mean of PHQ1 responses
#based on the groups
shamiri_phq1_mean <- shamiri_stats %>% group_by(Tribe,Gender,School,Age) %>% 
   summarise(phq1_mean = mean(PHQ1), .groups = 'drop') %>% as.data.frame()

print(shamiri_phq1_mean, row.names = FALSE)


#Grouping mode of PHQ1 responses by age, tribe, gender and school
#using dplyr group_by() to group on multiple columns and get the mean of PHQ1 responses
#based on the groups
shamiri_phq1_mode <- shamiri_stats %>% group_by(Tribe,Gender,School,Age) %>%
  summarize(phq1_mode = get_mode(PHQ1), .groups = 'drop') %>% as.data.frame()

print(shamiri_phq1_mode, row.names = FALSE)

#Grouping mean of PHQ2 responses by age, tribe, gender and school
#using dplyr group_by() to group on multiple columns and get the mean of PHQ2 responses
#based on the groups
shamiri_phq2_mean <- shamiri_stats %>% group_by(Tribe,Gender,School,Age) %>% 
  summarise(phq2_mean = mean(PHQ2), .groups = 'drop') %>% as.data.frame()

print(shamiri_phq2_mean, row.names = FALSE)

#Grouping mean of PHQ7 responses by age, tribe, gender and school
#using dplyr group_by() to group on multiple columns and get the mean of PHQ7 responses
#based on the groups
shamiri_phq7_mean <- shamiri_stats %>% group_by(Tribe,Gender,School,Age) %>% 
  summarise(phq7_mean = mean(PHQ7), .groups = 'drop') %>% as.data.frame()

print(shamiri_phq7_mean, row.names = FALSE)

#Finding the relationship of GAD across tribe, age, gender and school
#mean GAD response distribution across demographics
shamiri_gad_mean_dist <- shamiri_stats %>% group_by(Tribe,Gender,School,Age,School_Resources) %>%
  summarise(gad1_mean = mean(GAD1), gad2_mean = mean(GAD2),
            gad3_mean = mean(GAD3), gad4_mean = mean(GAD4), gad5_mean = mean(GAD5),
            gad6_mean = mean(GAD6), gad7_mean = mean(GAD7), .groups = 'drop') %>%
  as.data.frame()

print(shamiri_gad_mean_dist, row.names = FALSE)

#calculating average GAD score across all demographics with rowwise() function 
#to display result along the data
shamiri_gad_mean_dist_sum <- shamiri_gad_mean_dist %>% rowwise() %>%
  mutate(gadavg_score = sum(c_across(cols = starts_with('gad')))) %>% as.data.frame()

#drops <- c("avg_score")#using character vector to store column name to be dropped (duplicate column)
 
#shamiri_gad_mean_dist_sum = shamiri_gad_mean_dist_sum[,!names(shamiri_gad_mean_dist_sum) %in% drops]

print(shamiri_gad_mean_dist_sum, row.names = FALSE)




#visualizing the GAD response distribution by demographic
#using ggplot package from the tidyverse library
library(tidyverse)

#using a bar plot to visualize the findings based on tribe (Majority vs. Minority)
ggplot(data = shamiri_gad_mean_dist_sum, mapping = aes(x = Age, y = gadavg_score, fill = Gender)) +
  geom_bar(stat = "identity", width = 0.5, position = position_dodge()) + #for side-by-side positioning of bars
  facet_wrap(vars(Tribe)) +
  labs(title = "GAD Average score distribution for Tribes",
       x = "Age",
       y = "GAD Average Score") +
  theme_bw() +
  theme(axis.text.x = element_text(colour = "grey20", size = 12, angle = 90, hjust = 0.5, vjust = 0.5),
        axis.text.y = element_text(colour = "grey20", size = 12),
        strip.text = element_text(face = "italic"),
        text = element_text(size = 16))

#using a bar plot to visualize findings based on schools
ggplot(data = shamiri_gad_mean_dist_sum, mapping = aes(x = Age, y = gadavg_score, fill = Gender)) +
  geom_bar(stat = "identity", width = 0.5, position = position_dodge()) + #for side-by-side positioning of bars
  facet_wrap(vars(School)) +
  labs(title = "GAD Average score distribution for Schools",
       x = "Age",
       y = "GAD Average Score") +
  theme_bw() +
  theme(axis.text.x = element_text(colour = "grey20", size = 12, angle = 90, hjust = 0.5, vjust = 0.5),
        axis.text.y = element_text(colour = "grey20", size = 12),
        strip.text = element_text(face = "italic"),
        text = element_text(size = 16))

#using a bar plot to visualize findings based on school resources
ggplot(data = shamiri_gad_mean_dist_sum, mapping = aes(x = Age, y = gadavg_score, fill = Gender)) +
  geom_bar(stat = "identity", width = 0.5, position = position_dodge()) + #for side-by-side positioning of bars 
  facet_wrap(vars(School_Resources)) +
  labs(title = "GAD Average score distribution for School Resources",
       x = "Age",
       y = "GAD Average Score") +
  theme_bw() +
  theme(axis.text.x = element_text(colour = "grey20", size = 12, angle = 90, hjust = 0.5, vjust = 0.5),
        axis.text.y = element_text(colour = "grey20", size = 12),
        strip.text = element_text(face = "italic"),
        text = element_text(size = 16))


#analyzing and visualizing correlation between PHQ2 and PHQ7 with a bar plot
#phq2 visualization for Tribe distribution
ggplot(data = shamiri_phq2_mean, mapping = aes(x = Age, y = phq2_mean, fill = Gender)) +
  geom_bar(stat = "identity", width = 0.5, position = position_dodge()) + #for side-by-side positioning 
  facet_wrap(vars(Tribe)) +
  labs(title = "PHQ2 average response distribution per Tribe",
       x = "Age",
       y = "PHQ2 Average Score") +
  theme_bw() +
  theme(axis.text.x = element_text(colour = "grey20", size = 12, angle = 90, hjust = 0.5, vjust = 0.5),
        axis.text.y = element_text(colour = "grey20", size = 12),
        strip.text = element_text(face = "italic"),
        text = element_text(size = 16))

#phq7 visualization for Tribe distribution
ggplot(data = shamiri_phq7_mean, mapping = aes(x = Age, y = phq7_mean, fill = Gender)) +
  geom_bar(stat = "identity", width = 0.5, position = position_dodge()) + #for side-by-side positioning 
  facet_wrap(vars(Tribe)) +
  labs(title = "PHQ7 average response distribution per Tribe",
       x = "Age",
       y = "PHQ7 Average Score") +
  theme_bw() +
  theme(axis.text.x = element_text(colour = "grey20", size = 12, angle = 90, hjust = 0.5, vjust = 0.5),
        axis.text.y = element_text(colour = "grey20", size = 12),
        strip.text = element_text(face = "italic"),
        text = element_text(size = 16))

#phq2 visualization for School distribution
ggplot(data = shamiri_phq2_mean, mapping = aes(x = Age, y = phq2_mean, fill = Gender)) +
  geom_bar(stat = "identity", width = 0.5, position = position_dodge()) + #for side-by-side positioning 
  facet_wrap(vars(School)) +
  labs(title = "PHQ2 average response distribution per School",
       x = "Age",
       y = "PHQ2 Average Score") +
  theme_bw() +
  theme(axis.text.x = element_text(colour = "grey20", size = 12, angle = 90, hjust = 0.5, vjust = 0.5),
        axis.text.y = element_text(colour = "grey20", size = 12),
        strip.text = element_text(face = "italic"),
        text = element_text(size = 16))

#phq7 visualization for School distribution
ggplot(data = shamiri_phq7_mean, mapping = aes(x = Age, y = phq7_mean, fill = Gender)) +
  geom_bar(stat = "identity", width = 0.5, position = position_dodge()) + #for side-by-side positioning 
  facet_wrap(vars(School)) +
  labs(title = "PHQ7 average response distribution per School",
       x = "Age",
       y = "PHQ7 Average Score") +
  theme_bw() +
  theme(axis.text.x = element_text(colour = "grey20", size = 12, angle = 90, hjust = 0.5, vjust = 0.5),
        axis.text.y = element_text(colour = "grey20", size = 12),
        strip.text = element_text(face = "italic"),
        text = element_text(size = 16))

#count number of students in majority and minority tribes
shamiri_tribe_dist <- shamiri_stats %>% group_by(Tribe) %>% 
  count(ParticipantID, Tribe, .groups = 'drop') %>% as.data.frame()

print(shamiri_tribe_dist, row.names = FALSE)
