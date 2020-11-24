setwd("C:/Users/Kristin/Desktop/Class Documents/Safavi Work/Manifest 2")

soap.data <- read.csv("soap.csv")
shampoo.data <- read.csv("shampoo.csv")
toothpaste.data <- read.csv("toothpaste.csv")

for(d in 1:3){
  data = switch(d, soap.data, shampoo.data, toothpaste.data)
  par(mfrow = c(2,2))
  
  for(i in 2:dim(data)[2]){
    x = data$Year
    y = as.numeric(data[,i])
    plot(x, y, ylab = "Year", xlab = colnames(data)[i],
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
    plot(x, y, ylab = "Year", xlab = colnames(data)[i],
         main = switch(d, "soap", "shampoo", "toothpaste"))
    lines(x, y, xlim=range(x), ylim=range(y), pch=16)
  }
  
}
