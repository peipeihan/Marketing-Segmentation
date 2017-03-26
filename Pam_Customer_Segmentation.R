## Download "gdata" package if we want to read Excel file to R.

# library(gdata)

setwd("C:/Users/peipe/Desktop/BA1702/HW/hw5")

list.files()  # list all the files in current directory 

# Load data using read.xls 
# train_data = read.xls("Training_Data.xlsx", header = TRUE)
# OR 
# load .xlsx file by using read_excel in readxl library
# 
#install.packages("readxl") 

library(readxl)
train_data <-read_excel("Training_Data.xlsx")

# call in R packages for use in this study
library(lattice)  # multivariate data visualization
library(vcd)  # data visualization for categorical variables
library(cluster)  # cluster analysis methods

#install.packages("vcd")
#install.packages("cluster")
# examine the structure of the  train_data frame
str(train_data)
head(train_data)

names(train_data)

table(train_data$Gender)

unique(train_data$Gender)

summary(train_data$`Number of Adults`)

print(table(train_data$Gender , useNA = c("always")))
print(table(train_data$`Age Category` , useNA = c("always")))
print(table(train_data$`Race Category`, useNA = c("always")))
print(table(train_data$`Total House Income Category` , useNA = c("always")))
print(table(train_data$`Highest Education` , useNA = c("always")))
print(table(train_data$`Marital Status` , useNA = c("always")))

# select subset of variables for input to cluster analysis

#data_for_clustering <- subset(train_data,
#                              select = c("Gender", 
#                                         "Age Category", "Race Category", 
#                                         "Total House Income Category", "Highest Education",
#                                         "Employment Status",                    "Marital Status",                      
#                                         "Do you own or rent?",                  "Primary Residence",                   
#                                         "Square Footage of Home",               "Do you own a pool?",                  
#                                          "Number of Children under age of 18",   "Number of Adults",                    
#                                          "Number of regular residents",          "Who pays the bill?",                  
#                                          "Choice: Automated Monitoring",         "Choice: Power Line"   ,               
#                                          "Choice: Remote Control"      ,         "Choice: Lighting Control" ,           
#                                          "Choice: Alert System"         ,        "Choice: Automatic Billing"  ,         
#                                          "Choice: Email or Text Alert"   ,       "Choice: Phone Alert"  ,               
#                                          "Choice: Remote Monitoring System",     "Choice: Wind Power"   ,               
#                                          "Preference: Automated Monitoring",     "Preference: Power Line"  ,            
#                                          "Preference: Remote Control"    ,       "Preference: Lighting Control" ,       
#                                          "Preference: Alert System"       ,      "Preference: Automatic Billing"  ,     
#                                          "Preference: Email or Text Alert" ,     "Preference: Phone Alert" ,            
#                                          "Preference: Remote Monitoring System" ,"Preference: Wind Power"))


# select subset of variables for input to cluster analysis
data_for_clustering <- train_data[,-1]


# set file for graphical output from the clustering solutions
pdf(file = "fig_finding_new_cluster_search_9_12.pdf",    width = 8.5, height = 11)

# set clusters number range
min_clusters <- 9
max_clusters <- 12

# evaluate alternative numbers of clusters/segments
# we use the average silhouette width as a statistical criterion
evaluation_vector <- NULL  # initialize evaluation vector 

# selected algorithm is pam (partitioning around medoids)
# with so many binary variables, manhattan distances seemed to work better than Euclidean distances

for (number_of_clusters in min_clusters:max_clusters) {
  
  try_clustering <- pam(data_for_clustering, k = number_of_clusters,
                        metric = "manhattan", stand = TRUE)
  evaluation_vector <- rbind(evaluation_vector,
                             data.frame(number_of_clusters, 
                                        average_silhouette_width = 
                                          try_clustering$silinfo$avg.width))
  # show plot for this clustering solution
  plot(try_clustering)  # add this clustering solution to results file         
}        
dev.off()  # close the pdf results file for the clustering solution    

# examine the cluster solution results, 
# look for average silhouette width > 0.5 substantial 
# look for last big jump in average silhoutte width

print(evaluation_vector) 


# -----------------------------------------------------
# select clustering solution and examine it
# -----------------------------------------------------
# examine the seven-cluster solution in more detail
eleven_cluster_solution <- pam(data_for_clustering, k = 11,
                              metric = "manhattan", stand = TRUE)
pdf(file = "fig_finding_new_customers_11_cluster_solution.pdf",
    width = 8.5, height = 8.5)
plot(eleven_cluster_solution)
dev.off()

data_for_clustering$cluster = eleven_cluster_solution$clustering

unique(data_for_clustering$`Age Category`)
# look at demographics across the clusters/segments
# -----------------
# age  Age in years
# -----------------
# examine relationship between age and response to promotion
with(data_for_clustering, print(table(cluster, `Age Category`)))

# marital status
with(data_for_clustering, print(table(cluster, `Marital Status`)))


#table(data_for_clustering$Gender,data_for_clustering$`Age Category`)

