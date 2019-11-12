# Load and install necessary packages
requiredPackages <-
  c(
    "igraph",
    "ggplot2",
    "data.table",
    "knitr",
    "dplyr",
    "tidyr",
    "rstudioapi",
    "formattable",
    "purrr",
    "xtable",
    "stats4",
    # for MLE
    "VGAM"
  ) # for the Riemann-zeta function

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

source("./summary_table.R")

source %>%
  as.data.frame %>%
  setNames(c('language', 'file')) %>%
  mutate_if(is.factor, as.character) %>%
  mutate(summary = pmap(list(language, file), write_summary)) %>%
  unnest_wider(summary) %>%
  # removes extra columns from table
  select(-c('language', 'file')) %>%
  formattable(align = c("l", "c", "c", "c", "c", "c", "c", "c", "r"),
              list(`Language` = formatter(
                "span", style = ~ style(color = "black", font.weight = "bold")
              )))

source("./solution.r")

for (x in 1:nrow(source)) {
  print("-----------------------")
  print(source$language[x])
  estimate_likelihoods(source$file[x])
}

