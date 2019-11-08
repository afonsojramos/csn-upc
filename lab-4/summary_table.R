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
      
      summary_table <-
         rbind(summary_table, list(language, N, mu_n, sigma_n, mu_x, sigma_x))
   }
   return(summary_table)
}

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
