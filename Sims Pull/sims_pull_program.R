# library(knitr)
# library(rmarkdown)
# 
 setwd("C:/Users/rcarroll/Desktop/Git/Evaluation/Sims Pull")
# 
# 
#for (i in unique(sims_mock$OU_Name)) {
 for (i in unique(SIMSdatain$OU.Name)) {
   
  
  rmarkdown::render(input = "SIMS_High_level_analysis_pull.Rmd",
                    #params = list(OU_Name = i),
                    params = list(OU.Name = i),
                    output_format = "pdf_document",
                    output_file = paste0(i, ".pdf"))
}

