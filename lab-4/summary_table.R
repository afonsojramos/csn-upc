# Load and install necessary packages
requiredPackages <-
   c("igraph",
     "ggplot2",
     "data.table",
     "knitr",
     "rstudioapi",
     "xtable")

for (pac in requiredPackages) {
   if (!require(pac,  character.only = TRUE)) {
      install.packages(pac, repos = "http://cran.rstudio.com")
      library(pac,  character.only = TRUE)
   }
}
rm(pac)
rm(requiredPackages)

# set pwd to current directory, must load rstudioapi before.
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

source("./lab4.R")

## ---- Generate Metrics Table ----
## languages = languages to evaluate
write_summary <- function(languages) {
   summary_table <- data.table(
      "Language" = character(),
      "N" = numeric(),
      "mu_n" = numeric(),
      "sigma_n" = numeric(),
      "mu_x" = numeric(),
      "sigma_x" = numeric(),
      stringsAsFactors = FALSE
   )
   
   for (x in 1:length(languages)) {
      language <- languages[x]
      file <-
         paste(
            "./data/",
            language,
            "_dependency_tree_metrics.txt",
            sep = ""
         )
      
      language_values = read.table(file, header = FALSE)
      #cat(language,length(language_values$V1),max(language_values$V1),sum(language_values$V1)/length(language_values$V1),length(language_values$V1)/sum(language_values$V1),"\n")
      #compute n mu_n sigma_n mu_x sigma_x
      
      N = length(language_values$V1)
      mu_n = sum(language_values$V1)/length(language_values$V1) #mean of n
      sigma_n = 1/N * sum( (language_values$V1 - mu_n)^2 ) #standard deviation of n
      
      mu_x = sum(language_values$V2)/length(language_values$V2) #mean of <k^2>
      sigma_x = 1/N * sum( (language_values$V2 - mu_n)^2 ) #standard deviation of <k^2>
      
      #need to check validity : 4 - 6/n <= <k2> <= n - 1
      # and n/(8(n-1)) <k^2> + 1/2 <= <d> <= n - 1
      n = language_values$V1
      k2 = language_values$V2
      d = language_values$V3
      
      verified = TRUE
      for( i in length(n) ){
         if( !( (4 - 6/n[i] <= k2[i]) && ( k2[i] <= n[i] - 1 ) && ( n[i]/(8*(n[i]-1) ) * k2[i] + 0.5 <= d[i] ) && ( d[i] <= n[i] - 1 ) ) ){
            verified = FALSE
            break
         }   
      }
      
      if( verified ){
         print("metrics satisfy the conditions")
      }else{
         print("metrics doesn't satisfy the conditions")
      }
      
      summary_table <-
         rbind(summary_table, list(language, N, mu_n, sigma_n, mu_x, sigma_x))
   }
   return(summary_table)
}

languages = c("Catalan")
languages = c(
   "Arabic",
   "Basque",
   "Catalan",
   "Chinese",
   "Czech",
   "English",
   "Greek",
   "Hungarian",
   "Italian",
   "Turkish"
)

summary = write_summary(languages)
run_models(languages)

