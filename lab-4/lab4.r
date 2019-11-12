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

source("./summary_table.r")
source("./models.r")

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
tables = run_models(languages)
