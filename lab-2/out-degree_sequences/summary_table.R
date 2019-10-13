setwd("D:/Clouds/GitHub/csn-upc/lab-2/out-degree_sequences")
library(dplyr)
library(tidyr)

write_summary <- function(language,file) {
   degree_sequence = read.table(file, header = FALSE)
   return (c(Language = language,
             N = length(degree_sequence$V1), 
             'Maximum Degree' = max(degree_sequence$V1), 
             'M/N' = sum(degree_sequence$V1)/length(degree_sequence$V1), 
             'N/M' = length(degree_sequence$V1)/sum(degree_sequence$V1))
           )
}

source = read.table("./list.txt", 
         header = TRUE,               # this is to indicate the first line of the file contains the names of the columns instead of the real data
         as.is = c("language","file") # this is need to have the cells treated as real strings and not as categorial data.
        )

source %>%
    as.data.frame %>%
    setNames(c('language', 'file')) %>%
    mutate_if(is.factor, as.character) %>%
    mutate(summary = pmap(list(language, file), write_summary)) %>%
    # drop(language) %>%
    unnest_wider(summary)
