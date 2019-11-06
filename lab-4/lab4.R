write_summary <- function(language,file) {
   language_values = read.table(file, header = FALSE)
   
   colnames( language_values ) = c( "vertices" , "degree_2nd_moment" , "mean_lenght" )
   
   language = language[order(language_values$vertices)]
   
   cat( language_values )
   
}

source = read.table("list.txt", 
         header = TRUE,               # this is to indicate the first line of the file contains the names of the columns instead of the real data
         as.is = c("language","file") # this is need to have the cells treated as real strings and not as categorial data.
        )

for( x in 1:nrow(source) ){
    write_summary(source$language[x], source$file[x] )
}

"Arabic_dependency_tree_metrics.txt"

# loading datas
language_values = read.table("./data/Arabic_dependency_tree_metrics.txt", header = FALSE)
colnames( language_values ) = c( "vertices" , "degree_2nd_moment" , "mean_lenght" )
language_values = language_values[order(language_values$vertices), ]

# visualizing datas
plot(language_values$vertices, language_values$degree_2nd_moment, xlab = "vertices", ylab = "degree_2nd_moment" )

plot(log(language_values$vertices), log(language_values$degree_2nd_moment),xlab = "log(vertices)", ylab = "log(degree_2nd_moment)" )

mean_language = aggregate(language_values, list(language_values$vertices), mean)

plot(mean_language$vertices, mean_language$degree_2nd_moment,xlab = "vertices", ylab = "degree_2nd_moment")

plot(log(mean_language$vertices), log(mean_language$degree_2nd_moment),xlab = "log(vertices)", ylab = "log(degree_2nd_moment)")

plot(log(language_values$vertices), log(language_values$degree_2nd_moment),xlab = "vertices", ylab = "mean dependency length")*
lines(log(mean_language$vertices),log(mean_language$degree_2nd_moment), col = "green")
lines(log(mean_language$vertices),log((mean_language$vertices+1)/3), col = "red" )


plot(language_values$vertices, language_values$degree_2nd_moment, xlab = "vertices", ylab = "degree 2nd moment")

lines(mean_language$vertices,mean_language$degree_2nd_moment, col = "green")
lines(language_values$vertices,(1 - 1/language_values$vertices)*(5 - 6/language_values$vertices), col = "red")
lines(language_values$vertices,4-6/language_values$vertices, col = "blue")
lines(language_values$vertices,language_values$vertices-1, col = "blue" )


a_initial = 4
b_initial = 4
nonlinear_model = nls( degree_2nd_moment~a*vertices^b, data=language_values,start = list(a = a_initial, b = b_initial), trace = TRUE )

