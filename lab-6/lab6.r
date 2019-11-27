#### Install ####
requiredPackages <-
  c("igraph",
    "ggplot2",
    "data.table",
    "knitr",
    "rstudioapi",
    "minpack.lm",
    "xtable",
    "DT")

for (pac in requiredPackages) {
  if (!require(pac,  character.only = TRUE)) {
    install.packages(pac, repos = "http://cran.rstudio.com")
    library(pac,  character.only = TRUE)
  }
}

# You need to install this library manually
library(Rglpk) # https://stackoverflow.com/questions/25114771/glpk-no-such-file-or-directory-error-when-trying-to-install-r-package

rm(pac)
rm(requiredPackages)

# set pwd to current directory, must load rstudioapi before.
if(rstudioapi::isAvailable()) {
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
}

models = function(i) {
  
  # models without intercept
  model0 = nlsLM(i ~ a*t, start=list(a=1))
  model1 = nlsLM(i ~ a*sqrt(t),start=list(a=1))
  model2 = nlsLM(i ~ a*(t)^b, start = list(a=0.1, b=1))
  model3 = nlsLM(i ~ a*exp(c*t), start = list(a=1,c=0.0001))
  model4 = nlsLM(i ~ a*(log(abs(t+d1))), start=list(a=0.1 ,d1=1))
  
  # models with intercept
  model0i = nlsLM(i ~ a*t+d,start=list(a=1, d=1))
  model1i = nlsLM(i ~ a*sqrt(t)+d,start=list(a=1, d=1))
  model2i = nlsLM(i ~ a*((t)^b)+d, start = list(a=0.1, b=1, d=1), control=nls.lm.control(maxiter = 150))
  model3i = nlsLM(i ~ d+a*exp(c*t), start = list(a=1,c=0.01, d=0.1))
  model4i = nlsLM(i ~ a*(log(abs(t+d1)))+d2, start=list(a=10 ,d1=1000, d2=-100))
}
