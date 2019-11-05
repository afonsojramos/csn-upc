write_summary <- function(language,file) {
   language_values = read.table(file, header = FALSE)
   #cat(language,length(language_values$V1),max(language_values$V1),sum(language_values$V1)/length(language_values$V1),length(language_values$V1)/sum(language_values$V1),"\n")
   #compute n mu_n sigma_n mu_x sigma_x
   
   n = length(language_values$V1)
   mu_n = sum(language_values$V1)/length(language_values$V1) #mean of n
   sigma_n = 1/n * sum( (language_values$V1 - mu_n)^2 ) #standard deviation of n
   
   mu_x = sum(language_values$V2)/length(language_values$V2) #mean of <k^2>
   sigma_x = 1/n * sum( (language_values$V2 - mu_n)^2 ) #standard deviation of <k^2>
   
   cat(language,n,mu_n, sigma_n, mu_x, sigma_x,"\n")

}

source = read.table("list.txt", 
         header = TRUE,               # this is to indicate the first line of the file contains the names of the columns instead of the real data
         as.is = c("language","file") # this is need to have the cells treated as real strings and not as categorial data.
        )

for( x in 1:nrow(source) ){
    write_summary(source$language[x], source$file[x] )
}



# language = "Arabic"
# file = "./data/Arabic_dependency_tree_metrics.txt"
# 
# language_values = read.table(file, header = FALSE)
# cat(language,length(language_values$V1),max(language_values$V1),sum(language_values$V1)/length(language_values$V1),length(language_values$V1)/sum(language_values$V1),"\n")
