get_results <- function(language_values , nonlinear_model, model_name) {
   # get RSS, AIC and s
   RSS = deviance(nonlinear_model)
   AIC = AIC(nonlinear_model)
   s = sqrt(RSS / df.residual(nonlinear_model)) #s : residual standard error
   
   # get coefficient
   coeff =  coef(nonlinear_model)
   
   cat("RSS: ", RSS, "\nAIC: ", AIC, "\nS: ", s, "\ncoeff: ", coeff)
   
   # final plotting
   plot(
      log(language_values$vertices),
      log(language_values$degree_2nd_moment) ,
      xlab = "log(vertices)" ,
      ylab = "log(degree_2nd_moment)",
      main = model_name
   )
   lines(log(language_values$vertices), log(fitted(nonlinear_model)) , col = "green")
   
   return(list("RSS" = RSS, "AIC" = AIC, "S" = s, "coeff" = coeff)) 
   
}

run_models <- function(languages) {
   s_table <- data.table(
      "Language" = character(),
      "0" = numeric(),
      "1" = numeric(),
      "2" = numeric(),
      "3" = numeric(),
      "4" = numeric(),
      "1+" = numeric(),
      "2+" = numeric(),
      "3+" = numeric(),
      "4+" = numeric(),
      stringsAsFactors = FALSE
   )
   
   AIC_table <- data.table(
      "Language" = character(),
      "0" = numeric(),
      "1" = numeric(),
      "2" = numeric(),
      "3" = numeric(),
      "4" = numeric(),
      "1+" = numeric(),
      "2+" = numeric(),
      "3+" = numeric(),
      "4+" = numeric(),
      stringsAsFactors = FALSE
   )
   
   deltaAIC_table <- data.table(
      "Language" = character(),
      "0" = numeric(),
      "1" = numeric(),
      "2" = numeric(),
      "3" = numeric(),
      "4" = numeric(),
      "1+" = numeric(),
      "2+" = numeric(),
      "3+" = numeric(),
      "4+" = numeric(),
      stringsAsFactors = FALSE
   )
   
   values_table <- data.table(
      "Language" = character(),
      "1b" = numeric(),
      "2a" = numeric(),
      "2b" = numeric(),
      "3a" = numeric(),
      "3c" = numeric(),
      "4a" = numeric(),
      "1+b" = numeric(),
      "1+d" = numeric(),
      "2+a" = numeric(),
      "2+b" = numeric(),
      "2+d" = numeric(),
      "3+a" = numeric(),
      "3+c" = numeric(),
      "3+d" = numeric(),
      "4+a" = numeric(),
      "4+d" = numeric(),
      stringsAsFactors = FALSE
   )
   
   for (x in 1:length(languages)) {
      language <- languages[x]

      cat(
         "\n-----------------------------\n",
         language,
         "\n-----------------------------\n"
      )
      
      # loading datas
      language_values = read.table(paste(
         "./data/" ,
         language ,
         "_dependency_tree_metrics.txt" ,
         sep = ""
      ),
      header = FALSE)
      colnames(language_values) = c("vertices" , "degree_2nd_moment" , "mean_lenght")
      language_values = language_values[order(language_values$vertices),]
      
      
      # compute mean language
      mean_language = aggregate(language_values, list(language_values$vertices), mean)
      
      
      # ---- Checking Homocesdasticity ----
      
      # calculate variance in functions of number of vertices
      variances = aggregate(language_values, list(language_values$vertices), var)
      variances = variances[!apply(variances[, 3:4] == 0, 1, FUN = any, na.rm = TRUE), ] #remove zeros from dataframe
      variances = na.omit(variances) #remove na values
      
      # calculate Fmax
      Fmax = max(variances$degree_2nd_moment) / min(variances$degree_2nd_moment)
      deltaF = 0.5
      
      # check if Fmax is close to 1
      if (Fmax <= 1 + deltaF && delta >= 1 - deltaF) {
         print("data set is homocesdastic")
      } else{
         k = max(variances$Group.1) #get nb of groups
         nb_participants = data.frame(table(language_values$vertices)) # get nb of participants per group (actually the nb of participants is not the same in each group)
         n = mean(nb_participants$Freq) # to et over this issue, we just take the m of nb of participants per group, some other techniques could discuss
         # actually, the table goes only to k = 12 and n = 60, we surpass quite largely, so is it very significant ?
         # for k = 12 and n = 60, tables indicate Fmaxmax should be < 2.36
         cat("Fmax : " , Fmax )
      }
      
      
      # ---- Visualizing Data ----
      
      plot(
         language_values$vertices,
         language_values$degree_2nd_moment,
         xlab = "vertices",
         ylab = "degree_2nd_moment",
         main = paste(language,"- Preliminary Plot")
      )
      
      plot(
         log(language_values$vertices),
         log(language_values$degree_2nd_moment),
         xlab = "log(vertices)",
         ylab = "log(degree_2nd_moment)",
         main = paste(language,"- Preliminary Plot (with logs)")
      )
      
      plot(
         mean_language$vertices,
         mean_language$degree_2nd_moment,
         xlab = "vertices",
         ylab = "degree_2nd_moment",
         sub = "mean values",
         main = paste(language,"- Preliminary Plot (averaging mean lengths)")
      )
      
      
      plot(
         log(mean_language$vertices),
         log(mean_language$degree_2nd_moment),
         xlab = "log(vertices)",
         ylab = "log(degree_2nd_moment)",
         sub = "mean values",
         main = paste(language,"- Preliminary Plot (averaging mean lengths)")
      )
      
      # plot
      plot(
         log(language_values$vertices),
         log(language_values$degree_2nd_moment),
         xlab = "vertices",
         ylab = "log(degree_2nd_moment)",
         main = paste(language,"- Null Model")
      ) *
         lines(log(language_values$vertices), log((1 - 1 / language_values$vertices) *
                                                     (5 - 6 / (
                                                        language_values$vertices
                                                     ))), col = "green")
      # ---- Model 0 ----
      cat("\n----- Model 0 -----\n")
      
      model0RSS <- sum((language_values$degree_2nd_moment-(language_values$vertices+1)/3)^2)
      model0n <- length(language_values$vertices)
      p <- 0
      model0s <- sqrt(model0RSS/(model0n - p))
      model0AIC <- model0n*log(2*pi) + model0n*log(model0RSS/model0n) + model0n + 2*(p + 1)
      
      # ---- Model 1 ----
      cat("\n----- Model 1 -----\n")
      
      lm1 = lm(log(degree_2nd_moment) ~ log(vertices) , mean_language)
      
      print(lm1)
      
      b_initial = coef(lm1)[2]
      nonlinear_model = nls(
         degree_2nd_moment ~ (0.5) * vertices ^ b,
         data = mean_language,
         start = list(b = b_initial),
         trace = TRUE
      )
      
      model1 <- get_results(mean_language,  nonlinear_model , "Model 1" )
      
      # ---- Model 2 ----
      cat("\n----- Model 2 -----\n")
      
      lm2 = lm(log(degree_2nd_moment) ~ log(vertices) , mean_language)
      
      print(lm2)
      
      a_initial = exp(coef(lm2)[1])
      b_initial = coef(lm2)[2]
      nonlinear_model = nls(
         degree_2nd_moment ~ a * vertices ^ b,
         data = mean_language,
         start = list(a = a_initial, b = b_initial),
         trace = TRUE
      )
      
      model2 <- get_results(mean_language,  nonlinear_model, "Model 2")
      
      # ---- Model 3 ----
      cat("\n----- Model 3 -----\n")
      
      lm3 = lm(log(degree_2nd_moment) ~ vertices , mean_language)
      
      print(lm3)
      
      a_initial = exp(coef(lm3)[1])
      c_initial = coef(lm3)[2]
      
      #a_initial = 10
      #b_initial = 10
      
      nonlinear_model = nls(
         degree_2nd_moment ~ a * exp(c * vertices),
         data = mean_language,
         start = list(a = a_initial, c = c_initial),
         trace = TRUE
      )
      
      model3 <- get_results(mean_language,  nonlinear_model, "Model 3")
      
      # ---- Model 4 ----
      cat("\n----- Model 4 -----\n")
      
      lm4 = lm(degree_2nd_moment ~ log(vertices) , mean_language)
      
      print(lm4)
      
      a_initial = coef(lm4)[1]
      
      #a_initial = 10
      #b_initial = 10
      
      nonlinear_model = nls(
         degree_2nd_moment ~ a * log(vertices),
         data = mean_language,
         start = list(a = a_initial),
         trace = TRUE
      )
      
      model4 <- get_results(mean_language , nonlinear_model, "Model 4")
      
      # ---- Model 1+ ----
      cat("\n----- Model 1+ -----\n")
      
      lm1 = lm(log(degree_2nd_moment) ~ log(vertices) , mean_language)
      
      print(lm1)
      
      b_initial = coef(lm1)[2]
      d_initial = 0
      
      nonlinear_model = nls(
         degree_2nd_moment ~ (0.5) * vertices ^ b + d,
         data = mean_language,
         start = list(b = b_initial , d = d_initial),
         trace = TRUE
      )
      
      
      
      model1p <- get_results(mean_language,  nonlinear_model, "Model 1+")
      
      # ---- Model 2+ ----
      cat("\n----- Model 2+ -----\n")
      
      lm2 = lm(log(degree_2nd_moment) ~ log(vertices) , mean_language)
      
      print(lm2)
      
      a_initial = exp(coef(lm2)[1])
      b_initial = coef(lm2)[2]
      d_initial = 0
      
      nonlinear_model = nls(
         degree_2nd_moment ~ a * vertices ^ b + d,
         data = mean_language,
         start = list(a = a_initial, b = b_initial, d = d_initial),
         trace = TRUE,
         algorithm = "port",
         lower = 0
      )
      
      model2p <- get_results(mean_language,  nonlinear_model, "Model 2+")
      
      # ---- Model 3+ ----
      cat("\n----- Model 3+ -----\n")
      
      lm3 = lm(log(degree_2nd_moment) ~ vertices , mean_language)

      print(lm3)
      
      a_initial = exp(coef(lm3)[1])
      c_initial = coef(lm3)[2]
      d_initial = min(variances$degree_2nd_moment)
      
      #a_initial = 10
      #b_initial = 10
      
      nonlinear_model = nls(
         degree_2nd_moment ~ a * exp(c * vertices) + d,
         data = mean_language,
         start = list(a = a_initial, c = c_initial, d = d_initial),
         trace = TRUE,
         algorithm = "port",
         lower = 0
      )
      
      model3p <- get_results(mean_language,  nonlinear_model, "Model 3+")
      
      # ---- Model 4+ ----
      cat("\n----- Model 4+ -----\n")
      
      lm4 = lm(degree_2nd_moment ~ log(vertices) , mean_language)
      
      print(lm4)
      
      a_initial = coef(lm4)[1]
      d_initial = 0
      
      #a_initial = 10
      #b_initial = 10
      
      nonlinear_model = nls(
         degree_2nd_moment ~ a * log(vertices) + d,
         data = mean_language,
         start = list(a = a_initial, d = d_initial),
         trace = TRUE
      )
      
      model4p <- get_results(mean_language , nonlinear_model, "Model 4+")
      
      
      
      AIC_table <-
         rbind(AIC_table, list(language, model0AIC, model1$AIC, model2$AIC, model3$AIC, model4$AIC, model1p$AIC, model2p$AIC, model3p$AIC, model4p$AIC))
      
      s_table <-
         rbind(s_table, list(language, model0s, model1$S, model2$S, model3$S, model4$S, model1p$S, model2p$S, model3p$S, model4p$S))
      
      minAIC <- min(c(model0AIC, model1$AIC, model2$AIC, model3$AIC, model4$AIC, model1p$AIC, model2p$AIC, model3p$AIC, model4p$AIC))
      
      deltaAIC_table <-
         rbind(deltaAIC_table, list(language, model0AIC - minAIC, model1$AIC - minAIC, model2$AIC - minAIC, model3$AIC - minAIC, model4$AIC - minAIC, model1p$AIC - minAIC, model2p$AIC - minAIC, model3p$AIC - minAIC, model4p$AIC - minAIC))
   }
   
   return(list("AIC" = AIC_table, "S" = s_table, "deltaAIC" = deltaAIC_table))
}

