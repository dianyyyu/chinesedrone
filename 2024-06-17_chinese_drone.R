library(stargazer)
library(sandwich)
library(lmtest)
library(ggplot2)
library(ggpubr)
library(GGally)
library(stringr)
library(dplyr)
library(tidyr)
library(readxl)
library(xlsx)
library(car)
library(lme4)          # for multilevel models
library(Rcpp)
library(foreign)



# choose working directory
setwd("~/Desktop/")

excel_path1 <- "81uav_data.xlsx"
dat_81uav <- read_excel(excel_path1)
dat_81uav <- data.frame(dat_81uav)

# Function to extract only the English part of the column names
extract_english <- function(name) {
  # Extract the part after the last set of dots, if present
  parts <- unlist(strsplit(name, "\\.\\."))
  english_part <- parts[length(parts)]
  return(english_part)
}

# Apply the function to rename the columns
new_names <- sapply(names(dat_81uav), extract_english)
names(dat_81uav) <- new_names

# Drop the columns 'location' and 'Company.Name'
dat_81uav <- dat_81uav[ , !(names(dat_81uav) %in% c("location", "Company.Name"))]

# Function to remove dots and convert to lowercase
clean_names <- function(name) {
  gsub("\\.", "", tolower(name))
}

# Apply the function to rename the columns
names(dat_81uav) <- sapply(names(dat_81uav), clean_names)

# Display the headers to verify the changes
print(names(dat_81uav))

# manually change the typo of year data
dat_81uav$registrationyear[486] <- 2017
dat_81uav$registrationyear[1338] <- 2004
dat_81uav$registrationyear[148] <- 2019
dat_81uav$registrationyear[1437] <- 2011


########Value Chain Extraction######
# Initialize columns
dat_81uav$component <- 0
dat_81uav$service <- 0
dat_81uav$endproduct <- 0

# Assign values based on the conditions
dat_81uav$component <- ifelse(grepl("配件", dat_81uav$mainindustry), 1, 0)
dat_81uav$service <- ifelse(grepl("服务|培训", dat_81uav$mainindustry), 1, 0)
dat_81uav$endproduct <- ifelse(dat_81uav$service == 0 & dat_81uav$component == 0, 1, 0)


# Ensure the registrationyear column is numeric
dat_81uav$registrationyear <- as.numeric(dat_81uav$registrationyear)
# Count the number of NA values in the registeredyear column
num_na <- sum(is.na(dat_81uav$registrationyear))
# Print the result
print(num_na)
# Find the rows with NA in the registeredyear column
na_rows <- dat_81uav[is.na(dat_81uav$registrationyear), ]
# Print the rows with NA values (6 companies info not updated to 81uav.cn; wait til youuav.com info)
print(na_rows) 


######Histogram######
# Reshape the data to long format
dat_long <- dat_81uav %>%
  pivot_longer(cols = c(component, service, endproduct), 
               names_to = "type", 
               values_to = "count") %>%
  filter(count == 1)  # Only include rows where count is 1

# Plot combined histogram
ggplot(dat_long, aes(x = registrationyear, fill = type)) +
  geom_histogram(binwidth = 1, position = "dodge", color = "black") +
  scale_x_continuous(breaks = seq(1995, 2024, by = 2), limits = c(1995, 2024)) +
  labs(title = "Types of Firms vs Registration Year", x = "Registration Year", y = "Count", fill = "Type") +
  theme_minimal()

########Line Chart######
# Aggregate the total counts by registration year
total_counts <- dat_81uav %>%
  mutate(total = component + service + endproduct) %>%
  group_by(registrationyear) %>%
  summarize(component = sum(component),
            service = sum(service),
            endproduct = sum(endproduct),
            total = sum(total))

# Reshape the data to long format for plotting
dat_long <- total_counts %>%
  pivot_longer(cols = c(component, service, endproduct, total),
               names_to = "type",
               values_to = "count")

# Ensure that registrationyear is numeric and finite
dat_long <- dat_long %>%
  filter(!is.na(registrationyear) & is.finite(registrationyear))

# Plot the line chart
ggplot(dat_long, aes(x = registrationyear, y = count, color = type, group = type)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(limits = c(1995, 2024), breaks = seq(1995, 2024, by = 2)) +
  labs(title = "Types of Firms vs Registration Year", x = "Registration Year", y = "Count", color = "Type") +
  theme_minimal()


#####Cumulative line chart######
# Ensure the registrationyear column is numeric
dat_81uav$registrationyear <- as.numeric(dat_81uav$registrationyear)

# Aggregate the total counts by registration year
total_counts <- dat_81uav %>%
  mutate(total = component + service + endproduct) %>%
  group_by(registrationyear) %>%
  summarize(component = sum(component),
            service = sum(service),
            endproduct = sum(endproduct),
            total = sum(total))

# Compute the cumulative sums
total_counts <- total_counts %>%
  arrange(registrationyear) %>%
  mutate(cumulative_component = cumsum(component),
         cumulative_service = cumsum(service),
         cumulative_endproduct = cumsum(endproduct),
         cumulative_total = cumsum(total))

# Reshape the data to long format for plotting
dat_long <- total_counts %>%
  pivot_longer(cols = c(cumulative_component, cumulative_service, cumulative_endproduct, cumulative_total),
               names_to = "type",
               values_to = "count")

# Rename the types for better readability
dat_long$type <- recode(dat_long$type,
                        cumulative_component = "Component",
                        cumulative_service = "Service",
                        cumulative_endproduct = "End Product",
                        cumulative_total = "Total")

# Plot the cumulative line chart
ggplot(dat_long, aes(x = registrationyear, y = count, color = type, group = type)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(limits = c(1995, 2024), breaks = seq(1995, 2024, by = 2)) +
  labs(title = "Cumulative Types of Firms vs Registration Year", x = "Registration Year", y = "Cumulative Count", color = "Type") +
  theme_minimal()

print("hello")
