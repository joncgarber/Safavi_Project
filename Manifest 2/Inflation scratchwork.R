chamberCommerce = read.csv("M3_Chamber_of_commerce.csv", header = T)
hygeneData= read.csv("M3_Hygene_Data.csv", header = T)


totalIran = chamberCommerce[,1:6]
iranExim = chamberCommerce[,7:10]
totalTehran = chamberCommerce[,11:14]
tehranExim =  chamberCommerce[,15:18]


hygeneData


#Want to see if there is correleation between cardholders and imports?


hygeneData$Toothpaste.Import..Tons.




#measuring from 2010 - 2018
iranExim$Total.1[2:10]
hygeneData$Toothpaste.Import..Tons.[2:10]
plot(iranExim$Total.1[2:10]~hygeneData$Toothpaste.Import..Tons.[2:10])

cor(iranExim$Total.1[2:10]~hygeneData$Toothpaste.Import..Tons.[2:10])
iranExim$Total.1[2]
temp1<-droplevels(iranExim$Total.1[2:10])
temp1
####################################################################

time<-c(1,2,3,4,5,6,7,8,9)
tooth_diff<-c(-2946, -2991 ,-840,2727 ,800 ,765 ,1067 ,822 ,6417) 
plot(tooth_diff~time)

tooth_imp<-c(2964, 3560, 3742, 3607, 4984 ,4522, 3916, 4375, 213)
plot(tooth_imp~time)

soap_imp<-c(8752, 7880, 8103, 11126, 7636, 9477, 11111, 13354, 14372)
plot(soap_imp~time)
summary(lm(soap_imp~time))

#####################################################################



soap<-read.csv("soap.csv", header = T)
shampoo<-read.csv("shampoo.csv" , header = T)
toothpaste<-read.csv("toothpaste.csv" , header = T)

iran<-read.csv("iran_data.csv", header = T)
tehran<-read.csv("tehran_data.csv", header = T)
pop<-read.csv("Currency-population.csv", header  =T)

soap$Local.in.Tons
soap_total<-soap$Import.in.Tons[-1]+soap$Local.in.Tons[-1]
shampoo_total<-shampoo$Import.in.Tons+shampoo$Local.in.Tons
toothpaste_total<-toothpaste$Import.in.Tons+toothpaste$Local.in.Tons
par(mfrow =c(1,3))
plot(shampoo_total, type = "l")
plot(soap_total, type = "l")
plot(toothpaste_total, type = "l")

pop

inflation_vec = c()
for(i in 1:7){
  inflation_vec[i] = prod(pop$Inflation.Rate[(i+1):8]+1)
}
inflation_vec<-c(inflation_vec,1)


true_income<-inflation_vec*pop$GNI.Per.Capita.CBI.Rial...Current 
true_soap<-inflation_vec*soap$CIF.Value.per.Kilo.in.Rial
true_shampoo<-inflation_vec*shampoo$CIF.Value.per.Kilo.in.Rial
true_toothpaste<-inflation_vec*toothpaste$CIF.Value.per.Kilo.in.Rial

plot(true_income~true_soap, type = "l")


plot(true_income, type = "l")
plot(true_soap, type = "l")
plot(true_shampoo, type = "l")
plot(true_toothpaste,type = "l")
#even though income is going down and soap and shampoo going up relatively, they are still buying because they care about hygene 


############################################################################


soap$CIF.Value.per.Kilo.in.Rial
shampoo$CIF.Value.per.Kilo.in.Rial
toothpaste$CIF.Value.per.Kilo.in.Rial


library(car)
summary(lm(soap$CIF.Value.per.Kilo.in.Rial~shampoo$CIF.Value.per.Kilo.in.Rial + toothpaste$CIF.Value.per.Kilo.in.Rial))


summary(lm(log(pop$Total.Import.in.USD)~log(shampoo$CIF.Value.per.Kilo.in.Rial) + log(toothpaste$CIF.Value.per.Kilo.in.Rial) 
           +log(soap$CIF.Value.per.Kilo.in.Rial)))

expanded_soap<-c(11.67,13.98, 14.91, 15.16 , 15.44, 15.43, 15.53, 16.10, 16.07, 16.27, 16.39, 16.25, 16.50, 16.42, 16.32, 16.50, 16.83)
expanded_shampoo<-c(11.44, 12.97,13.66,14.26,15.03,15.28,15.93,16.63,16.93,16.65,16.86,17.13,16.82,17.12,17.13,17.41,17.80
)
expanded_toothpaste<-c(11.76,13.51,14.07,14.93,14.95,14.98,14.78,15.33,15.87,15.91,16.39,16.70,16.39,16.89,16.84,16.57,17.08
)
yearly_import<-c(20649741657,30470554310,39473583836,51446809177,71959429228,83731923449,111734544809,68433706484,91378822732,118652876282,
                 88254166983,66202570877,67655518138,41630331169,49171587983,69066994380,75842149734)
length(yearly_import)



trade_reg<-lm(log(yearly_import)~expanded_soap + expanded_toothpaste + expanded_shampoo)
####IMPORTANT#############
vif(trade_reg)
durbinWatsonTest(trade_reg)

#is variance constant?
bartlett.test(log(yearly_import),expanded_soap)
###########################
summary(lm(log(yearly_import)~expanded_soap))

plot((log(yearly_import)~expanded_soap))


transform_soap<-exp(expanded_soap)
plot(yearly_import~expanded_soap)



plot(sqrt(yearly_import)~transform_soap)
plot(yearly_import~sqrt(transform_soap))
summary(lm(yearly_import~sqrt(transform_soap)))

############Yearly Import is random###########################
library(nortest)
ad.test(yearly_import)
hist(yearly_import)
#############Is our response varable autocorrelated###########
acf(yearly_import)
library(car)
library(lmtest)
time<-seq(1,17)
durbinWatsonTest(lm(yearly_import~time))
#We see our data is autocorrelated

#are our variables autocorrelated?
durbinWatsonTest(lm(log(yearly_import)~time))
durbinWatsonTest(lm(expanded_soap~time))
durbinWatsonTest(lm(expanded_shampoo~time))
durbinWatsonTest(lm(expanded_toothpaste~time))

####################What if we do percentage change in datapoints?############
real_toothpaste<-exp(expanded_toothpaste)
real_soap<-exp(expanded_soap)
real_shampoo<-exp(expanded_shampoo)

lag_tooth<-real_toothpaste[1:16]
diff_toothpaste<-real_toothpaste[2:17] - lag_tooth

lag_soap<-real_soap[1:16]
diff_soap<-real_soap[2:17] - lag_soap

lag_shamp<-real_shampoo[1:16]
diff_shampoo<-real_shampoo[2:17] - lag_shamp


acf(diff_shampoo)
acf(diff_soap)
acf(diff_toothpaste)

durbinWatsonTest(lm(diff_toothpaste~seq(1,16)))
#This works!


yearly_import2<- yearly_import[2:17]
trade_reg = lm(yearly_import2 ~ diff_shampoo+diff_soap+diff_toothpaste)
summary(trade_reg)

plot(yearly_import2 ~ diff_shampoo)
summary(lm(yearly_import2 ~ diff_shampoo))

plot(yearly_import2 ~ diff_soap)
summary(lm(yearly_import2 ~ diff_soap))

plot(yearly_import2 ~ diff_toothpaste)
summary(lm(yearly_import2 ~ diff_toothpaste))


full<-lm(yearly_import2~diff_shampoo*diff_soap*diff_toothpaste )
magic_model<-step(lm(yearly_import2~1), scope = list(lower = ~1, upper = full), direction = 'both')
summary(magic_model)
#We see that the change in imports from year to year does not predict yearly import revenue 

#####What about predicting differences in revenue?################

diff_import<- yearly_import2 - yearly_import[1:16]


full2<-lm(diff_import~diff_shampoo*diff_soap*diff_toothpaste )
magic_model2<-step(lm(diff_import~1), scope = list(lower = ~1, upper = full2), direction = 'both')
summary(magic_model2)
#We see that the change in imports from year to year does not predict yearly import revenue 

####Now lets see if we can do some transformations

ratio_toothpaste<-real_toothpaste[2:17] / lag_tooth
ratio_soap<-real_soap[2:17] / lag_soap
ratio_shampoo<-real_shampoo[2:17] / lag_shamp
ratio_import<-yearly_import2 / yearly_import[1:16]

full3<-lm(ratio_import~ratio_toothpaste*ratio_soap*ratio_shampoo)
magic_model3<-step(lm(ratio_import~1), scope = list(lower = ~1, upper = full3), direction = 'both')
summary(magic_model3)
################################################################################################
full4<-lm(yearly_import[2:17]~ratio_toothpaste*ratio_soap*ratio_shampoo)
magic_model4<-step(lm(yearly_import[2:17]~1), scope = list(lower = ~1, upper = full4), direction = 'both')
summary(magic_model4)

plot(yearly_import[2:17]~log(ratio_toothpaste))
plot(yearly_import[2:17]~log(ratio_shampoo))
plot(yearly_import[2:17]~log(ratio_soap))

plot(log(yearly_import[2:17])~log(ratio_toothpaste))
plot(log(yearly_import[2:17])~log(ratio_shampoo))
plot(log(yearly_import[2:17])~log(ratio_soap))

summary(lm(log(yearly_import[2:17])~log(ratio_toothpaste)))
#################################################
#End of modeling
######################################
#What we have learned so far


#People have less to spend accounting for inflation
par(mforw = c(1,1))
plot(true_income, type = "l", main = "Inflation adjusted Income")

#The inflation adjusted CIF for importing shampoo, toothpaste, soap, has gone up
par(mfrow = c(1,3))
plot(true_soap, type = "l", main = "Inflation adjusted Soap CIF")
plot(true_shampoo, type = "l",main = "Inflation adjusted Shampoo CIF")
plot(true_toothpaste,type = "l", main = "Inflation adjusted Toothpaste CIF")

#