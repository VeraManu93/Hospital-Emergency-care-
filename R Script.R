

# Load required libraries
library(readxl)
library(writexl)
library(ggplot2)
library(tidyverse)

# Load the dataset (update the file path as needed)
file_path<-("Hospital Emergency care Dataset.xlsx")  
data <-read_excel(file_path)
View(data)



# Create a project in R. Write a script to perform the following tasks: 
# a. Open the data in R (name the dataframe as you choose). 
# b. Define two new variables (Year and Month) to specify the year and month of each observation: 
# i. [The substr() function can help with this task, combined with the mutate() function from the dplyr package]. 


data<- data|> 
  mutate(
    Year = substr(Período, 1, 4),      
    month = substr(Período, 6, 7),
    Month = case_when(
      month == "01" ~ "January",
      month == "02" ~ "February",
      month == "03" ~ "March",
      month == "04" ~ "April",
      month == "05" ~ "May",
      month == "06" ~ "June",
      month == "07" ~ "July",
      month == "08" ~ "August",
      month == "09" ~ "September",
      month == "10" ~ "October",
      month == "11" ~ "November",
      month == "12" ~ "December",
      TRUE ~ NA_character_
  )
  )
data<-select(data,-"month")
View(data)


#  Rename the variables to more intuitive names of your choice. 
data <- data %>% 
  rename(
    Date = "Período",
    Region = "Região",
    Institution = "Instituição",
    Geographic_location = "Localização Geográfica",
    
    Emergency_Attendance_SU_Manchester_Triage_Red  = "Nº_Atendimentos_em_Urgência_SU_Triagem_Manchester_Vermelha",
    Emergency_Attendances_SU_Manchester_Triage_Orange =  "Nº_Atendimentos_em_Urgência_SU_Triagem_Manchester_Laranja",
    Emergency_Attendances_SU_Manchester_Triage_Yellow =  "Nº_Atendimentos_em_Urgência_SU_Triagem_Manchester_Amarela",
    Emergency_Attendances_SU_Manchester_Triage_Green =  "Nº_Atendimentos_em_Urgência_SU_Triagem_Manchester_Verde",  
    Emergency_Attendances_SU_Manchester_Triage_Blue = "Nº_Atendimentos_em_Urgência_SU_Triagem_Manchester_Azul",
    Emergency_Attendances_SU_Manchester_Triage_White = "Nº_ Atendimentos_ em_Urgência_ SU_Triagem_Manchester_Branca",
    Attendances_Without_Manchester_Triage = "Nº_Atendimentos_s_Triagem_Manchester"
    
    
  )

View(data)


# Remove Geographic Location variable
data<-select(data,- "Geographic_location")
View(data)

# Define a new dataframe containing only records for the month of December from 2017 to 2023
data_december <- data %>% filter(Month == "December" & Year >= 2017 & Year <= 2023)
View(data_december)

# Create other variables that you consider relevant for the database you chose, 
data <- data |> 
  mutate( Total_Emergency_Cases = data$Emergency_Attendance_SU_Manchester_Triage_Red+data$Emergency_Attendances_SU_Manchester_Triage_Orange+data$Emergency_Attendances_SU_Manchester_Triage_Yellow+data$Emergency_Attendances_SU_Manchester_Triage_Green+data$Emergency_Attendances_SU_Manchester_Triage_Blue+data$Emergency_Attendances_SU_Manchester_Triage_White+data$Attendances_Without_Manchester_Triage * 100,)
View(data)

# For a variable of your choice, calculate the mean, standard deviation, and median separately for the years 2021 and 2023,
stats_2021 <- data %>% filter(Year == 2021) %>% summarise(
  Mean = mean(Emergency_Attendance_SU_Manchester_Triage_Red, na.rm = TRUE),
  SD = sd(Emergency_Attendance_SU_Manchester_Triage_Red, na.rm = TRUE),
  Median = median(Emergency_Attendance_SU_Manchester_Triage_Red, na.rm = TRUE)
)
View(stats_2021)


stats_2023 <- data %>% filter(Year == 2023) %>% summarise(
  Mean = mean(Emergency_Attendances_SU_Manchester_Triage_Green, na.rm = TRUE),
  SD = sd(Emergency_Attendances_SU_Manchester_Triage_Green, na.rm = TRUE),
  Median = median(Emergency_Attendances_SU_Manchester_Triage_Green, na.rm = TRUE)
)
View(stats_2023)

# Create a table showing the mean and standard deviation (for a variable of your choice) for the year 2022, broken down by health region 
stats_2022 <- data %>% filter(Year == "2022") %>% 
  group_by(Region) %>% 
  summarise(
    Mean = mean(Emergency_Attendance_SU_Manchester_Triage_Red, na.rm = TRUE),
    SD = sd(Emergency_Attendance_SU_Manchester_Triage_Red, na.rm = TRUE))
View(stats_2022)


# For an institution of your choice, create a graph illustrating its evolution from 2017 to 2023, also for a variable of your choice. 
institution_name <- "Centro Hospitalar Entre Douro e Vouga, EPE" 

# Filter data for the institution
data_institution <- data %>% filter(Institution == institution_name & !is.na(Emergency_Attendances_SU_Manchester_Triage_Green)& Year >= 2017 & Year <= 2023)
View(data_institution)


# Plot evolution of Emergency_Attendances_SU_Manchester_Triage_Green from 2017 to 2023
graph1 <- ggplot(data_institution, aes(x = Year, y = Emergency_Attendances_SU_Manchester_Triage_Green)) +
  geom_bar(stat="identity",color = "blue") +
  labs(title = paste("Evolution of Total Emergency Cases -", institution_name,"(2017-2023)"),
       x = "Date", y = "Emergency_Attendances_SU_Manchester_Triage_Green") +
  theme_minimal()
print(graph1)

# Save the graph as a PDF file.
pdf("Evolution of Total Emergency Cases_graph.pdf")
print(graph1)
dev.off()

# Save the constructed dataframe as an Excel file, with a name of your choice. 
write_xlsx(data_december,"Data_December.xlsx")
write_xlsx(data_institution,"Total_Emergency_Cases.xlsx")



