#import libraries
library(dplyr) #for data manipulation and transformation
library(ggplot2) #for data visualization
library(countrycode) #for converting country codes to country names
library(scales)  #library for formatting numbers and labels in plots


# Read the file using read.csv()
df <- read.csv("dataset.csv", header = TRUE)

# Print the contents of the data
head(df,2)

###### Data Preprocessing

print(colnames(df)) #list columns in the data frame

# Drop the column X
df <- subset(df, select = -c(X))
column_names <- colnames(df)
print(column_names)


# Checking null values
missing_values <- colSums(is.na(df))
print(missing_values)

# Function to check unique values in a column of a data frame
check_unique_values_in_column <- function(df, column_name) {
  unique_values <- unique(df[[column_name]])
  return(unique_values)
}

print(check_unique_values_in_column(df, "job_title")) # calling function to display unique values in Job_titles

# Recode specific job titles to "Data Scientist"
df$job_title <- recode(df$job_title,
                       "Data Science Engineer" = "Data Scientist",
                       "Data Science Manager" = "Data Scientist",
                       "Head of Data Science" = "Data Scientist")
                       "Principal Data Scientist" = "Data Scientist"
                       "Staff Data Scientist" = "Data Scientist"


print(check_unique_values_in_column(df, "experience_level")) # calling function for experience level
                       
# Replace values in the 'experience_level' column
df$experience_level <- recode(df$experience_level,
                              "EN" = "Entry-level/Junior",
                              "MI" = "Mid-level/Intermediate",
                              "SE" = "Senior-level/Expert",
                              "EX" = "Executive-level/Director")

print(check_unique_values_in_column(df, "employment_type")) # calling function for employment type

# Replace employment type abbreviations with full names
df$employment_type <- recode(df$employment_type,
                             "PT" = "Part Time",
                             "FT" = "Full Time",
                             "CT" = "Contract",
                             "FL" = "Freelance")

print(check_unique_values_in_column(df, "company_size")) # for uniques values in company size column

# Replace company_size abbreviations with full names
df$company_size <- recode(df$company_size,
                          "L" = "Large",
                          "M" = "Medium",
                          "S" = "Small")


print(check_unique_values_in_column(df, "remote_ratio")) # checking uniques in remote ratio

# Replace values in the "remote_ratio" column
df$remote_ratio <- recode(df$remote_ratio,
                          "0" = "Not Remote",
                          "50" = "Partially Remote",
                          "100" = "Fully Remote")


# Change company location code to full names
df$company_country <- countrycode(df$company_location, "iso2c", "country.name")

###### Data Visualization


##### Function to create a bar chart for the top values in a given column
create_top_column_plot <- function(data, column, top_n, title, x_label, y_label) {
  # Calculate the count of each unique value in the specified column
  column_grouped <- table(data[[column]])
  
  # Sort 'column_grouped' to get the top values
  top_values <- head(sort(column_grouped, decreasing = TRUE), top_n)
  
  # Create the bar chart using ggplot
  plot <- ggplot(data = data.frame(Value = names(top_values), Frequency = as.numeric(top_values)),
                 aes(x = reorder(Value, -Frequency), y = Frequency, fill = Value, label = Frequency)) +
    geom_bar(stat = "identity") +
    geom_text(position = position_stack(vjust = 0.5), color = "white", size = 4) +
    labs(title = title,
      x = x_label,
      y = y_label) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          axis.text = element_text(size = 12),
          axis.title = element_text(size = 14),
          plot.title = element_text(size = 18, hjust = 0.5))
  
  return(plot)
}
#calling function for top 5 job_titles
plot_job_title <- create_top_column_plot(data = df,
                                         column = "job_title",
                                         top_n = 5,
                                         title = "Top 5 Job Titles",
                                         x_label = "Job Title",
                                         y_label = "Frequency")

#calling function for top 5 company location
plot_location <- create_top_column_plot(data = df,
                                        column = "company_country",
                                        top_n = 5,
                                        title = "Top 5 Employer Company Locations",
                                        x_label = "Company Location",
                                        y_label = "Frequency")

print(plot_job_title)
print(plot_location)

##### Function to plot average salary for different job titles in top employer locations
plot_avg_salary_location <- function(df, job_title) {
  # Filter data for the specified job title
  job_data <- df[df$job_title == job_title, ]
  
  # Calculate the average salary for each unique company location
  average_salary_by_location <- aggregate(salary_in_usd ~ company_country, job_data, mean)
  
  # Sort the data by average salary in descending order
  average_salary_by_location <- average_salary_by_location[order(-average_salary_by_location$salary_in_usd), ]
  
  # Select only the top 10 employer locations
  top_10_locations <- head(average_salary_by_location, 10)
  
  # Convert average salary to 'k' format
  top_10_locations$salary_k <- top_10_locations$salary_in_usd / 1000
  
  plot <- ggplot(top_10_locations, aes(x = reorder(company_country, -salary_in_usd), y = salary_in_usd)) +
    geom_bar(stat = "identity", fill = "skyblue") +
    geom_text(aes(label = paste0("$", round(salary_k, 1), "k")), vjust = -0.5, size = 4, color = "black") +  # Add average salary labels in 'k' format
    labs(title = paste0("Average Salary of ", job_title, " - Top 10 Employer Locations"),
         x = "Company Location",
         y = "Average Salary (USD)") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          axis.text = element_text(size = 12),
          axis.title = element_text(size = 14),
          plot.title = element_text(size = 18, hjust = 0.5))
  
  # Display the plot
  print(plot)
}

plot_avg_salary_location(df, "Data Scientist") # function calling


##### Function to plot average salary for Data Scientists in the US vs Other countries over time
plot_avg_salary_over_time <- function(df, job_title) {
  # Filter data for the specified job title
  data_scientists <- df[df$job_title == job_title, ]
  
  # Calculate the average salary for data scientists in the United States
  average_salary_us <- aggregate(salary_in_usd ~ work_year, data_scientists[data_scientists$company_country == "United States", ], mean)
  
  # Calculate the average salary for data scientists in other countries
  average_salary_other <- aggregate(salary_in_usd ~ work_year, data_scientists[data_scientists$company_country != "United States", ], mean)
  
  # Convert average salary to 'k' format
  average_salary_us$salary_k <- average_salary_us$salary_in_usd / 1000
  average_salary_other$salary_k <- average_salary_other$salary_in_usd / 1000
  
  # Create the line plot using ggplot
  plot <- ggplot() +
    geom_line(data = average_salary_us, aes(x = as.Date(paste0(work_year, "-01-01")), y = salary_in_usd, color = "United States"), size = 1.5) +
    geom_line(data = average_salary_other, aes(x = as.Date(paste0(work_year, "-01-01")), y = salary_in_usd, color = "Other Countries"), size = 1.5) +
    geom_text(data = average_salary_us, aes(x = as.Date(paste0(work_year, "-01-01")), y = salary_in_usd, label = paste0("$", round(salary_k, 1), "k"), color = "United States"), vjust = -0.5, size = 4) +
    geom_text(data = average_salary_other, aes(x = as.Date(paste0(work_year, "-01-01")), y = salary_in_usd, label = paste0("$", round(salary_k, 1), "k"), color = "Other Countries"), vjust = -0.5, size = 4) +
    labs(title = paste0("Average Salary of ", job_title, " Over Time (United States vs Other Countries)"),
         x = "Year",
         y = "Average Salary (USD)",
         color = "Location") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          axis.text.y = element_text(size = 10),
          axis.title = element_text(size = 14),
          plot.title = element_text(size = 18, hjust = 0.5),
          legend.title = element_blank(),
          legend.position = "top")
  
  # Display the plot
  print(plot)
}

plot_avg_salary_over_time(df, "Data Scientist")


##### Function to plot remote ratio by year
plot_remote_ratio_by_year <- function(df, remote_ratio_column, work_year_column) {
  ggplot(df, aes(x = factor({{ remote_ratio_column }}, levels = c("Not Remote", "Partially Remote", "Fully Remote")), fill = factor({{ work_year_column }}))) +
    geom_bar(position = "dodge", width = 0.7) +
    theme_minimal() +
    labs(#title = "Remote Ratio by Year (2020, 2021, 2022)",
      x = "Remote Ratio",
      y = "Count") +
    scale_fill_discrete(name = "Year") +
    scale_x_discrete(labels = c("Not Remote", "Partially Remote", "Fully Remote"))
}

plot_remote_ratio_by_year(df, remote_ratio, work_year)


#####Function to create a pie chart of average salary by a specific column category for a given location 
create_pie_chart_by_location_and_column <- function(data, location, column_name) {
  # Filter the data based on the specified location
  if (location == "US") {
    df_location <- data %>%
      filter(company_location == "US", job_title == "Data Scientist")
  } else {
    df_location <- data %>%
      filter(company_location != "US", job_title == "Data Scientist")
  }
  # Calculate the average salary for each column category
  avg_salary_by_column <- df_location %>%
    group_by(.data[[column_name]]) %>%
    summarize(avg_salary = mean(salary_in_usd, na.rm = TRUE))
  print(avg_salary_by_column)
  # Create the pie chart
  ggplot(avg_salary_by_column, aes(x = "", y = avg_salary, fill = .data[[column_name]])) +
    geom_bar(stat = "identity", width = 1) +
    coord_polar(theta = "y") +
    geom_text(aes(label = paste(.data[[column_name]],"\n","$", comma(round(avg_salary / 1000, 1)), "k")), 
              position = position_stack(vjust = 0.5), color = "black", size = 4) +
    theme_minimal() +
    labs(title = paste("Average Salary for Data Scientists in", location,  "by", column_name),
         x = "",
         y = "",
         fill = column_name) +
    scale_fill_discrete(name = column_name) +
    theme(axis.text = element_blank(),  # Remove axis labels and ticks
          axis.ticks = element_blank(),
          axis.title = element_blank(),
          plot.title = element_text(size = 18, hjust = 0.5)) 
}

create_pie_chart_by_location_and_column(df, "US", "experience_level")
create_pie_chart_by_location_and_column(df, "Other", "experience_level")
create_pie_chart_by_location_and_column(df, "US", "remote_ratio")
create_pie_chart_by_location_and_column(df, "Other", "remote_ratio")
create_pie_chart_by_location_and_column(df, "US", "company_size")
create_pie_chart_by_location_and_column(df, "Other", "company_size")
create_pie_chart_by_location_and_column(df, "US", "employment_type")
create_pie_chart_by_location_and_column(df, "Other", "employment_type")


###### Function to create a visual representation of average salary for Data Scientists
#based on their experience level and remote ratio.
create_salary_visual_experience_remote <- function(data) {
  # Filter the data for data scientists
  df_data_scientist <- data %>%
    filter(job_title == "Data Scientist")
  
  # Calculate the average salary for each combination of experience level and remote ratio
  avg_salary_by_experience_remote <- df_data_scientist %>%
    group_by(experience_level, remote_ratio) %>%
    summarize(avg_salary = mean(salary_in_usd, na.rm = TRUE))
  
  # Create the visual
  ggplot(avg_salary_by_experience_remote, aes(x = experience_level, y = avg_salary, fill = remote_ratio)) +
    geom_bar(stat = "identity", position = "dodge") +
    geom_text(aes(label = paste("$", comma(round(avg_salary / 1000, 1)), "k")), 
              position = position_dodge(width = 0.9), vjust = -0.5) +
    theme_minimal() +
    labs(title = "Average Salary for Data Scientists by Experience Level and Remote Ratio",
         x = "Experience Level",
         y = "Average Salary",
         fill = "Remote Ratio") +
    scale_fill_discrete(labels = c("Not Remote", "Partially Remote", "Fully Remote")) +
    theme(legend.position = "bottom")  # Move legend to the bottom
}

df_data_scientists_us <- df[df$company_location == "US", ]  
df_data_scientists_other_than_us<- df[df$company_location != "US", ] 
create_salary_visual_experience_remote(df_data_scientists_other_than_us)
create_salary_visual_experience_remote(df_data_scientists_us)


###### Function to create a visual representation of average salary for Data Scientists
# with company size 'Small' by experience level in the US vs other countries.
create_salary_visual_experience_company_size_by_country <- function(data) {
  # Filter the data for data scientists with company size "S" in the US
  df_data_scientist_us <- data %>%
    filter(company_size == "Small", company_location == "US", job_title == "Data Scientist")
  
  # Filter the data for data scientists with company size "S" in other countries
  df_data_scientist_other <- data %>%
    filter(company_size == "Small", company_location != "US", job_title == "Data Scientist")
  
  # Calculate the average salary for each combination of experience level and country in the US
  avg_salary_by_experience_us <- df_data_scientist_us %>%
    group_by(experience_level) %>%
    summarize(avg_salary = mean(salary_in_usd, na.rm = TRUE))
  
  # Calculate the average salary for each combination of experience level and country in other countries
  avg_salary_by_experience_other <- df_data_scientist_other %>%
    group_by(experience_level) %>%
    summarize(avg_salary = mean(salary_in_usd, na.rm = TRUE))
  
  # Combine the data for US and other countries
  avg_salary_combined <- bind_rows(
    mutate(avg_salary_by_experience_us, company_location = "US"),
    mutate(avg_salary_by_experience_other, company_location = "Other")
  )
  
  # Create the visual
  ggplot(avg_salary_combined, aes(x = experience_level, y = avg_salary, fill = company_location)) +
    geom_bar(stat = "identity", position = "dodge") +
    geom_text(aes(label = paste("$", comma(round(avg_salary / 1000, 1)), "k")), 
              position = position_dodge(width = 0.9), vjust = -0.5) +
    theme_minimal() +
    labs(title = "Average Salary for Data Scientists with Company Size 'Small' by Experience Level(US vs Other Countries)",
         x = "Experience Level",
         y = "Average Salary",
         fill = "Company Location") +
    scale_fill_discrete(name = "Company Location") +
    theme(legend.position = "right",
          plot.margin = unit(c(1, 1, 1, 1), "lines"),
          axis.text.x = element_text(margin = margin(t = 25)))  
}

create_salary_visual_experience_company_size_by_country(df)


##### Function to create a multi-bar chart showing average salary of Data Scientists 
#in different locations by experience level
create_avg_salary_multi_bar_chart <- function(df, top_n) {
  # Calculate the average salary by company country and experience level
  average_salary_by_location_experience <- aggregate(salary_in_usd ~ company_country + experience_level, df, mean)
  
  # Find the top 'top_n' locations with the highest average salaries
  top_locations <- head(average_salary_by_location_experience[order(-average_salary_by_location_experience$salary_in_usd), ], top_n)
  
  # Filter the data to include only the top locations
  average_salary_top_locations <- subset(average_salary_by_location_experience, company_country %in% top_locations$company_country)
  
  # Convert average salary to 'k' format
  average_salary_top_locations$salary_k <- average_salary_top_locations$salary_in_usd / 1000
  
  # Create the multi-bar chart using ggplot
  plot <- ggplot(average_salary_top_locations, aes(x = company_country, y = salary_in_usd, fill = experience_level)) +
    geom_bar(stat = "identity", position = "dodge") +
    geom_text(aes(label = paste(round(salary_k, 1), "k")), position = position_dodge(width = 0.9), vjust = -0.5, size = 4, color = "black") +  # Add average value labels
    labs(title = paste("Average Salary of Data Scientists in different Countries by Experience Level"),
         x = "Company Location",
         y = "Average Salary (USD)") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          axis.text = element_text(size = 10),
          axis.title = element_text(size = 14),
          plot.title = element_text(size = 18, hjust = 0.5),
          legend.title = element_blank(),
          legend.position = "right")
  
  # Display the multi-bar chart
  print(plot)
}
df_data_scientists <- df[df$job_title == "Data Scientist", ]
create_avg_salary_multi_bar_chart(df_data_scientists, 12)


####### Function to create a multi-bar chart comparing the average salary of entry-level and mid-level data scientists 
#in the US and other countries for a Small Company
create_multi_bar_chart_small_company <- function(df) {
  #  Filter the data for entry-level and mid-level data scientists in the US and other countries
  filtered_data_entry_us <- df %>%
    filter(job_title == "Data Scientist",
           experience_level == "Entry-level/Junior",
           company_location == "US",
           employment_type == "Full Time",
           company_size == "Small")
  
  filtered_data_entry_other <- df %>%
    filter(job_title == "Data Scientist",
           experience_level == "Entry-level/Junior",
           company_location != "US",
           employment_type == "Full Time",
           company_size == "Small")
  
  filtered_data_mid_us <- df %>%
    filter(job_title == "Data Scientist",
           experience_level == "Mid-level/Intermediate",
           company_location == "US",
           employment_type == "Full Time",
           company_size == "Small")
  
  filtered_data_mid_other <- df %>%
    filter(job_title == "Data Scientist",
           experience_level == "Mid-level/Intermediate",
           company_location != "US",
           employment_type == "Full Time",
           company_size == "Small")
  
  # Calculate the average salary for entry-level and mid-level data scientists in the US and other countries
  average_salary_entry_us <- mean(filtered_data_entry_us$salary_in_usd, na.rm = TRUE)
  average_salary_entry_other <- mean(filtered_data_entry_other$salary_in_usd, na.rm = TRUE)
  
  average_salary_mid_us <- mean(filtered_data_mid_us$salary_in_usd, na.rm = TRUE)
  average_salary_mid_other <- mean(filtered_data_mid_other$salary_in_usd, na.rm = TRUE)
  
  # Create data frame for the multi-bar chart
  salary_comparison <- data.frame(Location = c("US", "US", "Other Countries", "Other Countries"),
                                  Experience_Level = c("Entry-level", "Mid-level", "Entry-level", "Mid-level"),
                                  Average_Salary = c(average_salary_entry_us, average_salary_mid_us,
                                                     average_salary_entry_other, average_salary_mid_other))
  
  # Create the multi-bar chart using ggplot
  plot <- ggplot(salary_comparison, aes(x = Location, y = Average_Salary, fill = Experience_Level)) +
    geom_col(position = "dodge") +
    geom_text(aes(label = paste0("$", round(Average_Salary / 1000, 1), "k")),
              position = position_dodge(width = 0.9), vjust = -0.5, size = 4, color = "black") +  # Add average salary label in 'k' format
    labs(title = "Average Salary of Data Scientists (Full Time, Small Company)",
      x = "Location",
      y = "Average Salary (USD)",
      fill = "Experience Level") +
    scale_fill_discrete(labels = c("Entry-level", "Mid-level")) +
    theme_minimal() +
    theme(axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12),
          axis.title = element_text(size = 14),
          plot.title = element_text(size = 18, hjust = 0.5),
          legend.title = element_blank())
  
  return(plot)
}

create_multi_bar_chart_small_company(df)


