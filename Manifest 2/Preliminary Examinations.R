

soap.data <- read.csv("soap.csv")
shampoo.data <- read.csv("shampoo.csv")
toothpaste.data <- read.csv("toothpaste.csv")
iran <- read.csv("iran_data.csv", header = T)
tehran <- read.csv("tehran_data.csv", header = T)

for(d in 1:3){
  data = switch(d, soap.data, shampoo.data, toothpaste.data)
  par(mfrow = c(2,2))
  
  for(i in 2:dim(data)[2]){
    x = data$Year
    y = as.numeric(data[,i])
    plot(x, y, xlab = "Year", ylab = colnames(data)[i],
         main = switch(d, "soap", "shampoo", "toothpaste"))
    lines(x, y, xlim=range(x), ylim=range(y), pch=16)
  }
 
}

for(i in 2:8){
  par(mfrow = c(1,3))
  
  for(d in 1:3){
    data = switch(d, soap.data, shampoo.data, toothpaste.data)
    x = data$Year
    y = as.numeric(data[,i])
    plot(x, y, xlab = "Year", ylab = colnames(data)[i],
         main = switch(d, "soap", "shampoo", "toothpaste"))
    lines(x, y, xlim=range(x), ylim=range(y), pch=16)
  }
  
}


#compare female cardholders to hygiene growth (Import in Tons)
par(mfrow = c(2,2))
x = iran$Year
y= iran$Female.Cardholders
plot(x,y, main = "Female Cardholders" )
lines(x, y, xlim=range(x), ylim=range(y), pch=16)

for(d in 1:3){
  data = switch(d, soap.data, shampoo.data, toothpaste.data)
  x = data$Year
  y = as.numeric(data$Import.in.Tons)
  plot(x, y, ylab = "Year", xlab = "Import in Tons",
       main = switch(d, "soap", "shampoo", "toothpaste"))
  lines(x, y, xlim=range(x), ylim=range(y), pch=16)
}


#compare female cardholders to hygiene growth (Domestic in Tons)
par(mfrow = c(2,2))
x = iran$Year
y= iran$Female.Cardholders
plot(x,y, main = "Female Cardholders" )
lines(x, y, xlim=range(x), ylim=range(y), pch=16)

for(d in 1:3){
  data = switch(d, soap.data, shampoo.data, toothpaste.data)
  x = data$Year
  y = as.numeric(data$Local.in.Tons)
  plot(x, y, ylab = "Year", xlab = "Local in Tons",
       main = switch(d, "soap", "shampoo", "toothpaste"))
  lines(x, y, xlim=range(x), ylim=range(y), pch=16)
}


#compare male cardholders to hygiene growth (Domestic in Tons)
par(mfrow = c(2,2))
x = iran$Year
y= iran$Male.Cardholders
plot(x,y, main = "Male Cardholders" )
lines(x, y, xlim=range(x), ylim=range(y), pch=16)

for(d in 1:3){
  data = switch(d, soap.data, shampoo.data, toothpaste.data)
  x = data$Year
  y = as.numeric(data$Local.in.Tons)
  plot(x, y, ylab = "Year", xlab = "Local in Tons",
       main = switch(d, "soap", "shampoo", "toothpaste"))
  lines(x, y, xlim=range(x), ylim=range(y), pch=16)
}


#compare male cardholders to hygiene growth (Import in Tons)
par(mfrow = c(2,2))
x = iran$Year
y= iran$Male.Cardholders
plot(x,y, main = "Male Cardholders" )
lines(x, y, xlim=range(x), ylim=range(y), pch=16)

for(d in 1:3){
  data = switch(d, soap.data, shampoo.data, toothpaste.data)
  x = data$Year
  y = as.numeric(data$Import.in.Tons)
  plot(x, y, ylab = "Year", xlab = "Import in Tons",
       main = switch(d, "soap", "shampoo", "toothpaste"))
  lines(x, y, xlim=range(x), ylim=range(y), pch=16)
}


#Compare Female Cardholder growth to Hygiene product growth
par(mfrow = c(2,2))
x = iran$Year
y= iran$ROG.Card.Female
plot(x,y, main = "ROG Female Cardholders" , ylim = c(0,2))
lines(x, y, xlim=range(x), ylim=range(y), pch=16)

for(d in 1:3){
  data = switch(d, soap.data, shampoo.data, toothpaste.data)
  x = data$Year
  y = as.numeric(data$ROG.Import)
  plot(x, y, ylab = "Year", xlab = " ROG Import in Tons",
       main = switch(d, "soap", "shampoo", "toothpaste"),ylim = c(0,2))
  lines(x, y, xlim=range(x), ylim=range(y), pch=16)
  
  correlation = cor(iran$ROG.Card.Female[-1], y[-1] )
  print(correlation)
}

#Compare Tehran Female Cardholder growth to Hygiene product growth
par(mfrow = c(2,2))
x = tehran$Year
y= tehran$ROG.Card.Female
plot(x,y, main = "Tehran ROG Female Cardholders" , ylim = c(0,2))
lines(x, y, xlim=range(x), ylim=range(y), pch=16)

for(d in 1:3){
  data = switch(d, soap.data, shampoo.data, toothpaste.data)
  x = data$Year
  y = as.numeric(data$ROG.Import)
  plot(x, y, xlab = "Year", ylab = " ROG Import in Tons",
       main = switch(d, "soap", "shampoo", "toothpaste"),ylim = c(0,2))
  lines(x, y, xlim=range(x), ylim=range(y), pch=16)
  
  correlation = cor(iran$ROG.Card.Female[-1], y[-1] )
  print(correlation)
}


#Compare Female Membership growth to Hygiene product growth
par(mfrow = c(2,2))
x = iran$Year
y= iran$ROG.Female.Member.Proportion
plot(x,y, main = "ROG Female Members" , ylim = c(0,2))
lines(x, y, xlim=range(x), ylim=range(y), pch=16)

for(d in 1:3){
  data = switch(d, soap.data, shampoo.data, toothpaste.data)
  x = data$Year
  y = as.numeric(data$ROG.Import)
  plot(x, y, ylab = "Year", xlab = " ROG Import in Tons",
       main = switch(d, "soap", "shampoo", "toothpaste"),ylim = c(0,2))
  lines(x, y, xlim=range(x), ylim=range(y), pch=16)
  
  correlation = cor(iran$ROG.Memb.Female[-1], y[-1] )
  print(correlation)
}
 

#Compare Female Membership Proportion to Hygiene product growth (local)
par(mfrow = c(2,2))
x = iran$Year
y= iran$ROG.Female.Member.Proportion
plot(x,y, main = "ROG Female Members Proportion" , ylim = c(0.5,3))
lines(x, y, xlim=range(x), ylim=range(y), pch=16)
abline(h=1, col = "blue")

for(d in 1:3){
  data = switch(d, soap.data, shampoo.data, toothpaste.data)
  x = data$Year
  y = as.numeric(data$ROG.Local)
  plot(x, y, ylab = "Year", xlab = " ROG Local in Tons",
       main = switch(d, "soap", "shampoo", "toothpaste"),ylim = c(0.5,3))
  lines(x, y, xlim=range(x), ylim=range(y), pch=16)
  abline(h=1, col = "blue")
  
  correlation = cor(iran$ROG.Female.Member.Proportion[-1], y[-1] )
  print(correlation)
}


plot(iran$Percent.Female.Members~iran$Year, type = "l",xlab = "year")
plot(shampoo.data$ROG.Import~shampoo.data$Year, type = "l",xlab = "year")
plot(soap.data$ROG.Import~shampoo.data$Year, type = "l",xlab = "year")
plot(toothpaste.data$ROG.Import~shampoo.data$Year, type = "l",xlab = "year")

plot(iran$Percent.Female.Cardholders~iran$Year, type = "l",xlab = "year")
plot(shampoo.data$ROG.Import~shampoo.data$Year, type = "l",xlab = "year")
plot(soap.data$ROG.Import~shampoo.data$Year, type = "l",xlab = "year")
plot(toothpaste.data$ROG.Import~shampoo.data$Year, type = "l",xlab = "year")

par(mfrow = (c(2,2)))
iran_female_cardgrowth<-iran$Percent.Female.Cardholders[2:8] / iran$Percent.Female.Cardholders[1:7]
plot(iran_female_cardgrowth~shampoo.data$Year[-1], type = "l")
plot(shampoo.data$ROG.Import~shampoo.data$Year, type = "l",xlab = "year")
plot(soap.data$ROG.Import~shampoo.data$Year, type = "l",xlab = "year")
plot(toothpaste.data$ROG.Import~shampoo.data$Year, type = "l",xlab = "year")

cor(iran_female_cardgrowth,shampoo.data$ROG.Import[-1])
cor(iran_female_cardgrowth,soap.data$ROG.Import[-1])
cor(iran_female_cardgrowth,toothpaste.data$ROG.Import[-1])

length(iran_female_cardgrowth)
length(shampoo.data$ROG.Import)
iran_female_cardgrowth

##################################################
#What we have learned so far from these

#Shampoo imports make up small percentage of total legal shampoo (lines: 9-21)

#see the growth of local and marketshare with all 3 

#Female and Male cardholders in IRAN ROG relatively similar (lines: 55-87)

#Female card holder proportion does not seem to significantly influence ROG imports for products (lines: 187-190)

