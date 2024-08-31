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
library(RColorBrewer)

# Reading in data
rankings <- read.csv("Creamery_Rankings.csv")

# Remove two columns
rankings <- rankings |> 
  dplyr::select(-Timestamp, -Email)
  
# View full dataset
View(rankings)

# Head of data
head(rankings)

# Factorization
rankings <- rankings |> mutate_at(c('Role', 'I.am'), as.factor)

#--------------------------------------------------------------------------------
# EDA - Exploratory Data Analysis 
# Getting a feel for how the data looks/is

# Reshape data
long_rankings <- rankings %>%
  pivot_longer(
    cols = starts_with("Blue.Goggles"):starts_with("Sparkle.Sherbet"),
    names_to = "Flavor",
    values_to = "Rank"
  )

# What is the proportion of faculty vs. student?
ggplot(data=rankings, mapping=aes(x=Role, fill=Role)) + 
  geom_bar() + ggtitle("Proportion of Faculty vs. Students Participants") + 
  xlab("Count") + ylab("Particpants")

# Boxplot
ggplot(long_rankings, aes(Flavor, Rank, fill=Flavor)) +
  geom_boxplot(width=0.7) +
  ggtitle("Boxplot of Rankings by Flavor") +
  theme(axis.text.x=element_text(angle=90, hjust=1))

# Creating a tibble of means per flavor to show in a table 
means.df <- long_rankings %>%
  group_by(Flavor) %>%
  summarize(Mean=round(mean(Rank),2)) %>%
  arrange(Mean)

gt(means.df) |> 
  tab_header(
    title = md("**Average Ranking for Each Flavor of Ice Cream**"),
    subtitle = "Faculty and Student Scores Included"
  )

# Creating a tibble of means per flavor considering Role
means_role.df <- long_rankings %>%
  group_by(Flavor, Role) %>%
  summarize(Mean=round(mean(Rank),2)) %>%
  arrange(Mean)

gt(means_role.df) |> 
  tab_header(
    title = md("**Average Ranking for Each Flavor of Ice Cream**"),
    subtitle = "Seperated by Faculty and Student Scores"
  )



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

# Question: Is there a difference between all students and all faculty 
#average ranking of Strawberry Sundae Crunch?

# Explanatory variable (x): Role (either student or faculty)
# Quantitative variable (y): Rank

# Separating student and faculty rankings
student_rankings <- rankings |> 
  filter(Role == "Student")

faculty_rankings <- rankings |> 
  filter(Role == "Faculty")

# Histograms by flavor
one <- ggplot(data=rankings, mapping=aes(x=Blue.Goggles)) +
  geom_histogram(position='identity', binwidth=0.5, alpha=0.5, fill="red") +
  ggtitle("Blue Googles") + xlab("Rank")
two <- ggplot(data=rankings, mapping=aes(x=Cookies...Cream)) + geom_histogram(position='identity', binwidth=0.5, alpha=0.5, fill="orange") + ggtitle("Cookies and Cream") + xlab("Rank")
three <- ggplot(data=rankings, mapping=aes(x=Earnestly.Chocolate)) + geom_histogram(position='identity', binwidth=0.5, alpha=0.5, fill="yellow") + ggtitle("Earnestly Chocolate") + xlab("Rank")
four <- ggplot(data=rankings, mapping=aes(x=Graham.Canyon)) + geom_histogram(position='identity', binwidth=0.5, alpha=0.5, fill="darkolivegreen1") + ggtitle("Graham.Canyon") + xlab("Rank")
five <- ggplot(data=rankings, mapping=aes(x=Lemon.Berry.Cheesecake)) + geom_histogram(position='identity', binwidth=0.5, alpha=0.5, fill="springgreen4") + ggtitle("Lemon Berry Cheesecake") + xlab("Rank")
six <- ggplot(data=rankings, mapping=aes(x=Mint.Chocolate.Chip)) + geom_histogram(position='identity', binwidth=0.5, alpha=0.5, fill="aquamarine3") + ggtitle("Mint Chocolate Chip") + xlab("Rank")
seven <- ggplot(data=rankings, mapping=aes(x=Peanut.Butter.Trails)) + geom_histogram(position='identity', binwidth=0.5, alpha=0.5, fill="steelblue2") + ggtitle("Peanut Butter Trails") + xlab("Rank")
eight <- ggplot(data=rankings, mapping=aes(x=Pralines...Caramel)) + geom_histogram(position='identity', binwidth=0.5, alpha=0.5, fill="blue") + ggtitle("Pralines and Caramel") + xlab("Rank")
nine <- ggplot(data=rankings, mapping=aes(x=Raspberry.and.Cream.Cheese)) + geom_histogram(position='identity', binwidth=0.5, alpha=0.5, fill="mediumpurple") + ggtitle("Raspberry and Cream Cheese") + xlab("Rank")
ten <- ggplot(data=rankings, mapping=aes(x=Roasted.Almond.Fudge)) + geom_histogram(position='identity', binwidth=0.5, alpha=0.5, fill="mediumorchid1") + ggtitle("Roasted Almond Fudge") + xlab("Rank")
eleven <- ggplot(data=rankings, mapping=aes(x=Strawberry.Sundae.Crunch)) + geom_histogram(position='identity', binwidth=0.5, alpha=0.5,  fill="violetred1") + ggtitle("Strawberry Sundae Crunch") + xlab("Rank")
twelve <- ggplot(data=rankings, mapping=aes(x=Sparkle.Sherbet)) + geom_histogram(position='identity', binwidth=0.5, alpha=0.5, fill ="hotpink") + ggtitle("Sparkle Sherbet") + xlab("Rank")

#This is all the histograms! Is our data normal?
one + two + three + four + five + six + seven + eight + nine + ten + eleven + twelve

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

# Great, lets do the t.test now
# Change x and y based on which flavor you want to test. 
t.test(x = student_rankings$Strawberry.Sundae.Crunch, y=faculty_rankings$Strawberry.Sundae.Crunch, alternative = "two.sided", mu = 0, paired = FALSE, var.equal = TRUE,
       conf.level = 0.95)

# From the t-test, we draw a conclusion! The p was low in comparison to .05!

# Given the p-value of 0.0456 and the alpha level of 0.05, we reject 
# the null hypothesis and conclude that there is a difference in the 
# average Strawberry Sundae Crunch ranking between faculty and students.
#--------------------------------------------------------------------------------
#2. Cluster Students based on ice cream preference

# Might be smart to run this just in case 
dev.off()

set.seed(123)

# Compute distance between students based on rankings
suppressWarnings(distance_matrix <- dist(as.matrix(rankings), method = "euclidean"))

# Just making sure there is no NAS
sum(is.na(distance_matrix))

# Hierarchical clustering
clustering <- hclust(distance_matrix, method = "complete")

# With the student name
plot(clustering, rankings$Name, main = "Hierarchical Clustering of Student Creamery Rankings", xlab = "Students", ylab = "Distance")

#Cut the dendrogram to create six clusters
num_clusters <- 10
cluster_labels <- cutree(clustering, k = num_clusters)

# Add the cluster assignments to your original data
rankings$Cluster <- cluster_labels

ClusterNumber <- rankings |>
  dplyr::select(Cluster, Name) |>
  group_by(Cluster) |>
  dplyr::summarize(Names = paste(Name, collapse = ", "), .groups = 'drop') |>
  arrange(Cluster)

ClusterNumber |> 
  kbl() |> 
  kable_styling()

#Yay! Time to meet your ice cream buddy :)
