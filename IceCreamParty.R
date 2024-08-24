# Alexia's Notes
#1. Should we do less ice cream flavors if we 87 students coming? 
#2. Might need to normalize faculty data...
#3. Created some extra tables/graphs but can be deleted
#4. Might want to make histogram different color that is easier to see?

# Get working directory
getwd()

# Install Necessary Packages
devtools::install_github("thomasp85/patchwork")

# Load Libraries
library(tidyverse)
library(ggplot2)
library(gt)
library(stats)
library(patchwork)
library(hrbrthemes)
library(knitr)
library(gt)
library(RColorBrewer) #scale_fill_brewer(palette="Dark2"), might want to do different color histograms

# Reading in data
rankings <- read.csv("Creamery_Rankings.csv")
rankings <- read.csv("Creamery Rankings - Actual.csv")

rankings <- rankings |> 
  dplyr::select(-Timestamp)
  
# View full dataset
View(rankings)

# Head of data
head(rankings)

# Factorization
rankings <- rankings |> mutate_at(c('Role', 'Sex'), as.factor)

#--------------------------------------------------------------------------------
# EDA - Exploratory Data Analysis 
# Getting a feel for how the data looks/is

# Reshape data
long_rankings <- rankings |>
  pivot_longer(cols = starts_with("Ice.Cream"),
               names_to = "Flavor",
               values_to = "Rank")


# Boxplot
ggplot(long_rankings, aes(Role, Rank, fill=Flavor)) +
  geom_boxplot(width=0.7) +
  ggtitle("Boxplot of Student vs. Faculty Rankings Filled by Flavor")

# Print Numerical Summary for rank...except it is obvious
favstats(~Rank, data=long_rankings)

#Real n, above is just different because using long version of data
favstats(~Rank, data=long_rankings)$n / 7

# Creating a tibble of means per flavor to show in a table 
means.df <- tibble(One = mean(rankings$Ice.Cream..1), Two = mean(rankings$Ice.Cream..2), 
Three = mean(rankings$Ice.Cream..3), Four = mean(rankings$Ice.Cream..4),Five = mean(rankings$Ice.Cream..5), 
Six = mean(rankings$Ice.Cream..6), Seven = mean(rankings$Ice.Cream..7))

gt(means.df) |> 
  tab_header(
    title = md("**Average Ranking for Each Flavor of Ice Cream**"),
    subtitle = "Faculty and Student Scores Included"
  )


# What is the proportion of faculty vs student?
ggplot(data=rankings, mapping=aes(x=Role, fill=Role)) + 
  geom_bar() + ggtitle("Proportion of Faculty vs. Students Participants") + 
  xlab("Count") + ylab("Particpants")


# What about for sex? 
ggplot(data=rankings, mapping=aes(x=Sex, fill=Sex)) + 
  geom_bar() + ggtitle("Proportion of Participants by Sex") + 
  xlab("Sex") + ylab("Sex")

# Create heatmap, can be by name or we could do it by roll of faculty vs. students
ggplot(long_rankings, aes(x = Flavor, y = Name, fill = Rank)) +
  geom_tile(color ="white") +
  scale_fill_distiller(palette = "RdPu") +
  labs(title = "Heatmap of Ice Cream Flavor Rankings",
       x = "Ice Cream Flavor",
       y = "Individual",
       fill = "Rank") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Rotate x-axis labels
#--------------------------------------------------------------------------------
# T-test between student and faculty average ranking of an ice cream flavor

# Question: Is there a difference between the student and faculty 
#average ranking of an ice cream flavor?


# Null Hypothesis: There is no difference between the average faculty and student ranking of
# the selected ice cream flavor. 
# Alternative Hypothesis: There is a difference between the average faculty and student ranking
# of the selected ice cream flavor. 


# Explanatory variable (x): Role (either student or faculty)
# Quantitative variable (y): Rank


# In order to do a t-test, there are conditions that must be checked
# 1. Distribution is normal
# 2. Similar variance in each group


# Separating student and faculty rankings again
student_rankings <- rankings |> 
  filter(Role == "Student")

faculty_rankings <- rankings |> 
  filter(Role == "Faculty")


# Density Plot for Student Rankings
ggplot() + 
  geom_density(data = student_rankings, mapping=aes(x=Ice.Cream..1, color="Ice Cream #1")) + 
  geom_density(data = student_rankings, mapping=aes(x=Ice.Cream..2, color="Ice Cream #2")) + 
  geom_density(data = student_rankings, mapping=aes(x=Ice.Cream..3, color="Ice Cream #3")) + 
  geom_density(data = student_rankings, mapping=aes(x=Ice.Cream..4, color="Ice Cream #4")) + 
  geom_density(data = student_rankings, mapping=aes(x=Ice.Cream..5, color="Ice Cream #5")) + 
  geom_density(data = student_rankings, mapping=aes(x=Ice.Cream..6, color="Ice Cream #6")) + 
  geom_density(data = student_rankings, mapping=aes(x=Ice.Cream..7, color="Ice Cream #7")) +
  ggtitle("Density Plot of Student Rankings by Flavor") + 
  xlab("Rank") + ylab("Density") + labs(color = "Flavor")

# Density Plot for Faculty Rankings
ggplot() + 
  geom_density(data = faculty_rankings, mapping=aes(x=Ice.Cream..1, color="Ice Cream #1")) + 
  geom_density(data = faculty_rankings, mapping=aes(x=Ice.Cream..2, color="Ice Cream #2")) + 
  geom_density(data = faculty_rankings, mapping=aes(x=Ice.Cream..3, color="Ice Cream #3")) + 
  geom_density(data = faculty_rankings, mapping=aes(x=Ice.Cream..4, color="Ice Cream #4")) + 
  geom_density(data = faculty_rankings, mapping=aes(x=Ice.Cream..5, color="Ice Cream #5")) + 
  geom_density(data = faculty_rankings, mapping=aes(x=Ice.Cream..6, color="Ice Cream #6")) + 
  geom_density(data = faculty_rankings, mapping=aes(x=Ice.Cream..7, color="Ice Cream #7")) +
  ggtitle("Density Plot of Faculty Rankings by Flavor") + 
  xlab("Rank") + ylab("Density") + labs(color = "Flavor")
 

# Histograms by flavor
one <- ggplot(data=rankings, mapping=aes(x=Ice.Cream..1, fill=Role)) + 
  geom_histogram(position='identity', binwidth=0.5, alpha=0.5) +
  ggtitle("Ice Cream #1") + xlab("Rank")
two <- ggplot(data=rankings, mapping=aes(x=Ice.Cream..2, fill=Role)) + 
  geom_histogram(position='identity', binwidth=0.5, alpha=0.5) +
  ggtitle("Ice Cream #2") + xlab("Rank")
three <- ggplot(data=rankings, mapping=aes(x=Ice.Cream..3, fill=Role)) + 
  geom_histogram(position='identity', binwidth=0.5, alpha=0.5) +
  ggtitle("Ice Cream #3") + xlab("Rank")
four <- ggplot(data=rankings, mapping=aes(x=Ice.Cream..4, fill=Role)) + 
  geom_histogram(position='identity', binwidth=0.5, alpha=0.5) +
  ggtitle("Ice Cream #4") + xlab("Rank")
five <- ggplot(data=rankings, mapping=aes(x=Ice.Cream..5, fill=Role)) + 
  geom_histogram(position='identity', binwidth=0.5, alpha=0.5) +
  ggtitle("Ice Cream #5") + xlab("Rank")
six <- ggplot(data=rankings, mapping=aes(x=Ice.Cream..6, fill=Role)) + 
  geom_histogram(position='identity', binwidth=0.5, alpha=0.5) +
  ggtitle("Ice Cream #6") + xlab("Rank")
seven <- ggplot(data=rankings, mapping=aes(x=Ice.Cream..7, fill=Role)) + 
  geom_histogram(position='identity', binwidth=0.5, alpha=0.5) +
  ggtitle("Ice Cream #7") + xlab("Rank")

#This is all the histograms! Is our data normal? Do we need CLT?
one + two + three + four + five + six + seven


# Use long rankings to check standard deviations of both groups 
long_faculty <- long_rankings |> 
  dplyr::select(Role, Rank) |> 
  filter(Role == "Faculty") 

long_students <- long_rankings |> 
  dplyr::select(Role, Rank) |> 
  filter(Role == "Student") 

# Checking standard deviation are close enough
# Max(s1, s2) / min (s1, s2) < 2
sd(long_faculty$Rank) 
sd(long_students$Rank) 
# max()/min() 
# Is it appropriate to continue with our t-distribution? 

# Great, lets do the t.test now
# Change x and y based on which flavor you want to test. 
t.test(x = student_rankings$Ice.Cream..6, y=faculty_rankings$Ice.Cream..6, alternative = "two.sided", mu = 0, paired = FALSE, var.equal = TRUE,
       conf.level = 0.95)

# From the t-test, we draw a conclusion! Was p low or high?

#--------------------------------------------------------------------------------
#2. Cluster Students based on ice cream preference

# Might be smart to run this just in case 
dev.off()

# Get rankings for only the students
student_rankings <- rankings |> 
  filter(Role == "Student") |> 
  group_by(Name) |> 
  dplyr::select(starts_with("Ice.Cream"))

# Compute distance between students based on rankings
suppressWarnings(distance_matrix <- dist(student_rankings, method = "euclidean"))

# Just making sure there is no NAS
sum(is.na(distance_matrix))

# Hierarchical clustering
clustering <- hclust(distance_matrix, method = "complete")

# Plotting below

# With the student number
plot(clustering, main = "Hierarchical Clustering of Student Creamery Rankings", xlab = "Students", ylab = "Distance")

# With the student name
plot(clustering, student_rankings$Name, main = "Hierarchical Clustering of Student Creamery Rankings", xlab = "Students", ylab = "Distance")

#Yay! Time to meet your ice cream buddy :)