# Abidjan DQA combined files


# Load Libraries ------------------
if(!require("tidyverse")) install.packages("tidyverse") 
if(!require("readxl")) install.packages("readxl")
if(!require("janitor")) install.packages("janitor") 
if(!require("data.table")) install.packages("data.table")
if(!require("lubridate")) install.packages("lubridate")


# Change Directory Names HERE ----------------

user_name <- "rcarroll"  #Change to your user name here

site_name <- "Hopital General de Port Bouet Day 2" # change this to the site name

#Set Directories ----------------------------
data_pull <- paste0(("C:/Users/"),user_name,("/Desktop/Abidjan DQA/Sites/"),site_name) 

final_directory <- paste0(("C:/Users/"),user_name,("/Desktop/Abidjan DQA/Combined_files from R"))

#Create a list of all of the excel files
file_names <- list.files(data_pull) 

#Make sure we only have the files we want in our folder. Remove any that are not the filled out files for the facility 

print(file_names)


setwd(data_pull)
# Grab all of the file names so we can create a column that tells us which file each row belongs to
all_filenames <- file_names %>%
  basename() %>%
  as.list() 

all_sheets = lapply(file_names, function(i){
  x = read_xlsx(i, sheet = "Entry Sheet", skip = 12, col_types = c("text", "text", "date", "text", "text", "date", "text", "text", "text"))
}) 

all_sheets2 <- mapply(c, all_sheets, all_filenames, SIMPLIFY = FALSE) 

# Clean the data and put into the final format we need -----------------------------
all_sheets3<- 
  rbindlist(all_sheets2, fill = TRUE, use.names = TRUE) %>%
  filter(!is.na(`Numero Unique d'Identification`)) %>%
  mutate(`Date de la derniere visite clinique  avant le 1er juillet 2022(jj/mm/aaaa)` = ymd(`Date de la derniere visite clinique  avant le 1er juillet 2022(jj/mm/aaaa)`)) %>%   
  mutate(`Date de l'evenement avant le 1er juillet 2022(jj/mm/aaaa)` = ymd(`Date de l'evenement avant le 1er juillet 2022(jj/mm/aaaa)`))

all_sheets4 <-  all_sheets3 %>%
  mutate(`Num. d'Ordre` = 1:nrow(all_sheets3)) %>%
  replace(is.na(all_sheets3), "" ) 


#Export the sheet to our final folder -----------------------------------
write_csv(all_sheets4, sprintf(paste0(("%s/"),site_name,("_Combined_Final_.csv")), final_directory))


