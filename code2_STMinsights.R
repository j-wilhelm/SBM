#install packages
install.packages("devtools")
install.packages("stmBrowser")
install.packages("bindrcpp")
install.packages("Rcpp")
install.packages("igraph")
install.packages("tibble")
install.packages("scales")

library(devtools)
#this function works with all R packages that were created in github
devtools::install_github('methodds/stminsights')

library(stminsights)
#before running the function, be sure that you have the .Rdata in your pc!
run_stminsights()  #now, this is ready. Just run the function

#load(file=paste0(getwd(),"/output/","MASHABLEstmshares20.RData"))