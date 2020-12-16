setwd("C:/Users/garbe/Desktop/Class Documents/Safavi Project/Manifest 2/Formatted Data")
library(car)
library(olsrr)
library(nortest)

tonsToKilos <- function(tons){
  return(1000*tons)
}

calcTotalCIF <- function(CIF.per.kilo, total.tons, inflation.vector = rep(0,8)){
  
  inflation.vec = c()
  for(i in 1:7){
    inflation.vec[i] = prod(inflation.vector[(i+1):8]+1)
  }
  inflation.vec<-c(inflation.vec,1)
  
  total.kilos = tonsToKilos(total.tons)
  total.CIF = inflation.vec * CIF.per.kilo * total.kilos
  
  return(total.CIF)
}


basicPlots <- function(model, main = ""){
  resids = model$residuals
  plot(t(model$model[1]) ~ t(model$model[2]), 
       xlab = colnames(model$model)[2], ylab = colnames(model$model[1]), main = main)
  abline(model$coefficients[1], model$coefficients[2])
  plot(resids, ylab = "Residuals")
  abline(0,0, col = 'blue', lty = 2)
}



runTests <- function(model, show = TRUE){
  #only works for single variable
  dw.cv = c(0.61, 0.7, 0.763, 0.824, 0,879, 0927)
  
  resids = model$residuals
  ad = ad.test(resids)
  dw = durbinWatsonTest(model)$p
  bp = ols_test_breusch_pagan(model)
  f = ols_test_f(model)
  t.p = summary(model)$coefficients[2,4]
  
  pass.ad = ad$p.value > 0.05
  pass.dw = dw > 0.05
  pass.bp = bp$p > 0.05
  pass.f = f$p > 0.05
  pass.t = t.p <= 0.05
  
  if(show == TRUE){
    cat(paste(
      "\nBeta_1 (t test):", round(t.p,5),
      "\nPasses Association Assumption:", pass.t,
      "\n---------------------------------------------",
      "\nDurbin-Watson Test:", round(dw,5),
      "\nPasses Independence Assumption:", pass.dw,
      "\n---------------------------------------------",
      "\nAnderson-Darling Test:", round(ad$p.value,5), 
      "\nPasses Normality Assumption:", pass.ad,
      "\n---------------------------------------------",
      "\nBreusch Pagan Test:", round(bp$p, 5), 
      "\nPasses Homoskedasticity Assumption:", pass.bp,
      "\n---------------------------------------------",
      "\nF Test:", round(f$p, 5), 
      "\nPasses Homoskedasticity Assumption:", pass.f, "\n",
      sep = " "))
  }
  
  invisible(return(pass.t && pass.ad && pass.f && pass.bp && pass.dw))
  
}

differencing <- function(Vector){
  newVector <- vector[ 2:(length(Vector)) ] - vector[ 1:(length(vector)-1) ]
  return(newVector)
}

lagRatio <- function(Vector){
  newVector <- vector[ 2:(length(Vector)) ] / vector[ 1:(length(vector)-1) ]
  return(newVector)
}


tryTransform <- function(var1, var2){
  diff1 = differencing(var1)
  diff2 = differencing(var2)
  ratio1 = lagRatio(var1)
  ratio2 = lagRatio(var2)
  
  if(invisible(runTests(lm(var1~var2), show = FALSE))){
    return(lm(var1 ~ var2))
  }
  else if(invisible(runTests(lm( log(var1)~var2), show = FALSE ))){
    return(lm(log(var1)~var2))
  }
  else if(invisible(runTests( lm( log(var1)~log(var2) ), show = FALSE ))){
    return(lm( log(var1)~log(var2) ))
  }
  else if(invisible(runTests( lm( var1~log(var2) ), show = FALSE ))){
    return(lm( var1~log(var2) ))
  }
  
  else if(invisible(runTests(lm(diff1~diff2), show = FALSE))){
    return(lm(diff1 ~ diff2))
  }
  else if(invisible(runTests(lm( log(diff1)~diff2), show = FALSE ))){
    return(lm(log(diff1)~diff2))
  }
  else if(invisible(runTests( lm( log(diff1)~log(diff2) ), show = FALSE ))){
    return(lm( log(diff1)~log(diff2) ))
  }
  else if(invisible(runTests( lm( diff1~log(diff2) ), show = FALSE ))){
    return(lm( diff1~log(diff2) ))
  }
  
  else if(invisible(runTests(lm(ratio1~ratio2), show = FALSE))){
    return(lm(ratio1 ~ ratio2))
  }
  else if(invisible(runTests(lm( log(ratio1)~ratio2), show = FALSE ))){
    return(lm(log(ratio1)~ratio2))
  }
  else if(invisible(runTests( lm( log(ratio1)~log(ratio2) ), show = FALSE ))){
    return(lm( log(ratio1)~log(ratio2) ))
  }
  else if(invisible(runTests( lm( ratio1~log(ratio2) ), show = FALSE ))){
    return(lm( ratio1~log(ratio2) ))
  }
  
  else{
    return(NULL)
  }
  
  
}

###########################################################
#Defining Data Variables

soap.data <- read.csv("soap.csv")
shampoo.data <- read.csv("shampoo.csv")
toothpaste.data <- read.csv("toothpaste.csv")
iran <- read.csv("iran_data.csv", header = T)
tehran <- read.csv("tehran_data.csv", header = T)
pop <- read.csv("Currency-population.csv", header = T)
Year = pop$Year
inflation <- pop$Inflation.Rate
soap.cif =  calcTotalCIF(soap.data$CIF.Value.per.Kilo.in.Rial, 
                         soap.data$Import.in.Tons)
shamp.cif = calcTotalCIF(shampoo.data$CIF.Value.per.Kilo.in.Rial, 
                         shampoo.data$Import.in.Tons)
tooth.cif = calcTotalCIF(toothpaste.data$CIF.Value.per.Kilo.in.Rial, toothpaste.data$Import.in.Tons)
soap.cif.adj =  calcTotalCIF(soap.data$CIF.Value.per.Kilo.in.Rial, 
                         soap.data$Import.in.Tons, inflation)
shamp.cif.adj = calcTotalCIF(shampoo.data$CIF.Value.per.Kilo.in.Rial, 
                         shampoo.data$Import.in.Tons, inflation)
tooth.cif.adj = calcTotalCIF(toothpaste.data$CIF.Value.per.Kilo.in.Rial, 
                             toothpaste.data$Import.in.Tons, inflation)
combined.cif = soap.cif + shamp.cif + tooth.cif
combined.cif.adj = soap.cif.adj + shamp.cif.adj + tooth.cif.adj
combined.tons = soap.data$Import.in.Tons + 
  shampoo.data$Import.in.Tons + toothpaste.data$Import.in.Tons


##################################################################

# Association between total imports to each hygiene import

par(mfrow = c(2,2))

#Total Import in $ VS each hygiene product import in Tons
plot(Year, pop$Total.Import.in.USD, type = "l", ylab = "Total Imports in USD")
plot(Year, soap.data$Import.in.Tons, type = "l", ylab = "Soap Import in Tons")
plot(Year, shampoo.data$Import.in.Tons, type = "l", ylab = "Shampoo Import in Tons")
plot(Year, toothpaste.data$Import.in.Tons, type = "l", ylab = "Toothpaste Import in Tons")


#Total Import in $ VS each hygiene product in Rial
plot(Year, pop$Total.Import.in.USD, type = "l", 
     ylab = "Total Imports in USD")
plot(Year, soap.cif, type = "l", ylab = "Soap Import in Rial")
plot(Year, shamp.cif, type = "l", ylab = "Shampoo Import in Rial")
plot(Year, tooth.cif, type = "l", ylab = "Toothpaste Import in Rial")


#Total Import in $ VS each hygiene product in Rial (adjusted for inflation)
plot(Year, pop$Total.Import.in.USD, type = "l", ylab = "Total Imports in USD")
plot(Year, soap.cif.adj, type = "l", ylab = "Soap Import in (Adj) Rial")
plot(Year, shamp.cif.adj, type = "l", ylab = "Shampoo Import in (Adj) Rial")
plot(Year, tooth.cif.adj, type = "l", ylab = "Toothpaste Import in (Adj) Rial")


#total imports VS combined hygiene in rial
par(mfrow = c(1,2))

plot(Year, pop$Total.Import.in.USD, type = "l", 
     ylab = "Total Imports in USD")
plot(Year, combined.cif, 
     type = "l", ylab = "Combined Import in Rial")


plot(Year, pop$Total.Import.in.USD, type = "l", 
     ylab = "Total Imports in USD")
plot(Year, combined.cif.adj, 
     type = "l", ylab = "Combined Import in Rial (adjusted)")


plot(Year, pop$Total.Import.in.USD, type = "l", 
     ylab = "Total Imports in USD")
plot(Year, combined.tons, 
     type = "l", ylab = "Combined Import in Tons")






#durban Watson would ideally output 2, 
#anything close to 0 or 4 signifies autocorrelation
#Durbin-Watson should be performed on residuals


###########################################################################
###########################################################################
#Models of Hygene Imports (in tons) vs Total Import in USD

soap.tons.model = lm(pop$Total.Import.in.USD ~ soap.data$Import.in.Tons)
basicPlots(soap.tons.model, main = "Total Import VS Soap Tons")
runTests(soap.tons.model)
tryTransform(pop$Total.Import.in.USD, soap.data$Import.in.Tons)


shamp.tons.model = lm(pop$Total.Import.in.USD ~ shampoo.data$Import.in.Tons)
basicPlots(shamp.tons.model, main = "Total Import VS Shampoo Tons")
runTests(shamp.tons.model)
tryTransform(pop$Total.Import.in.USD, shampoo.data$Import.in.Tons)


#ACCEPTABLE
tooth.tons.model = lm(pop$Total.Import.in.USD ~ toothpaste.data$Import.in.Tons)
basicPlots(tooth.tons.model, main = "Total Import VS Toothpaste Tons")
runTests(tooth.tons.model)
tryTransform(pop$Total.Import.in.USD, toothpaste.data$Import.in.Tons)



#---------------------------------------------------------
#Total Import in $ VS each hygiene product in Rial


soap.cif.model = lm(pop$Total.Import.in.USD ~ soap.cif)
basicPlots(soap.cif.model, main = "Total Import VS Soap Total CIF")
runTests(soap.cif.model)
model = tryTransform(pop$Total.Import.in.USD, soap.cif)

shamp.cif.model = lm(pop$Total.Import.in.USD ~ shamp.cif)
basicPlots(shamp.cif.model, main = "Total Import VS Shampoo Total CIF")
runTests(shamp.cif.model)
model = tryTransform(pop$Total.Import.in.USD, shamp.cif)

tooth.cif.model = lm(pop$Total.Import.in.USD ~ tooth.cif)
basicPlots(tooth.cif.model, main = "Total Import VS Toothpaste Total CIF")
runTests(tooth.cif.model)
tryTransform(pop$Total.Import.in.USD, tooth.cif)


#--------------------------------------------------------
#Total Import in $ VS each hygiene product in Rial (adjusted for inflation)

soap.cif.adj.model = lm(pop$Total.Import.in.USD ~ soap.cif.adj)
basicPlots(soap.cif.adj.model, main = "Total Import VS Soap Total CIF(adj)")
runTests(soap.cif.adj.model)
tryTransform(pop$Total.Import.in.USD, soap.cif.adj)

shamp.cif.adj.model = lm(pop$Total.Import.in.USD ~ shamp.cif.adj)
basicPlots(shamp.cif.adj.model, main = "Total Import VS Shampoo Total CIF(adj)")
runTests(shamp.cif.adj.model)
tryTransform(pop$Total.Import.in.USD, shamp.cif.adj)


tooth.cif.adj.model = lm(pop$Total.Import.in.USD ~ tooth.cif.adj)
basicPlots(tooth.cif.adj.model, main = "Total Import VS Toothpaste Total CIF(adj)")
runTests(tooth.cif.adj.model)
tryTransform(pop$Total.Import.in.USD, tooth.cif.adj)

#-----------------------------------------------------
#total imports VS combined hygiene 



combined.cif.model = lm(pop$Total.Import.in.USD ~ combined.cif)
basicPlots(combined.cif.model, main = "Total Import VS Combined Total CIF")
runTests(combined.cif.model)
tryTransform(pop$Total.Import.in.USD, combined.cif)


combined.cif.adj.model = lm(pop$Total.Import.in.USD ~ combined.cif.adj)
basicPlots(combined.cif.adj.model, main = "Total Import VS Combined Total CIF(adj)")
runTests(combined.cif.adj.model)
tryTransform(pop$Total.Import.in.USD, combined.cif.adj)

combined.tons.model = lm(pop$Total.Import.in.USD ~ combined.tons)
basicPlots(combined.tons.model, main = "Total Import VS Combined Total Tons")
runTests(combined.tons.model)
tryTransform(pop$Total.Import.in.USD, combined.tons)

