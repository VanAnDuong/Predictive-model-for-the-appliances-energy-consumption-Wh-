poly_data <- log_data
for(i in 1:6) {
  poly_data[, i] <- (poly_data[,i]^2)
}


#Stad
stad_data <- log_data
for(i in 1:6) {
  stad_data[, i] <- 0.15*((stad_data[,i] - mean(stad_data[,i])) / sd(stad_data[,i])) + 0.5
}

#Transform input
X[1] <- log(X[1])
X[2] <- (X[2])^3
X[3] <- log(X[3])
X[4] <- log(X[4])

X1 <- (X[1]-min(log_data[,1]))/(max(log_data[,1])-min(log_data[,1])) 
X2 <- 1 - (X[2]-min(log_data[,2]))/(max(log_data[,2])-min(log_data[,2]))   
X3 <- (X[3]-min(log_data[,3]))/(max(log_data[,3])-min(log_data[,3])) 
X4 <- (X[4]-min(log_data[,4]))/(max(log_data[,4])-min(log_data[,4])) 

X_nor <- c(X1, X2, X3, X4)

Y_pre <- OWA(X_nor,weight)

Y_original_scale = min(log_data[,5]) + (max(log_data[,5]) - min(log_data[,5])) * Y_pre

Y_predicted<-exp(Y_original_scale)

#Transform input
for (i in 1:4) { 
  if (i == 2) { #If it is X2
    X[i] <- (X[i])^3 #Apply cube for it
  } else { #Others 
    X[i] <- log(X[i]) #Apply log transformation for them
  }
}
#Min max
for(i in 1:4) {
  X[i] <- (X[i]-min(log_data[,1]))/(max(log_data[,i])-min(log_data[,i]))
}
#Negation X2
X[2] <- 1 - X[2] #1 is a max value from (0,1)

#Transform input
X[1] <- log(X[1])
X[2] <- (X[2])^3
X[3] <- log(X[3])
X[4] <- log(X[4])

X[1] <- (X[1]-min(log_data[,1]))/(max(log_data[,1])-min(log_data[,1])) 
X[2] <- 1 - (X[2]-min(log_data[,2]))/(max(log_data[,2])-min(log_data[,2]))   
X[3] <- (X[3]-min(log_data[,3]))/(max(log_data[,3])-min(log_data[,3])) 
X[4] <- (X[4]-min(log_data[,4]))/(max(log_data[,4])-min(log_data[,4])) 





#Task 2
#It is hard to choose variables, so I will use correlation 
correlations <- numeric(5) #define a vector contains 5 results
for (i in 1:5) {
  correlations[i] <- cor(my.data[, i], my.data[, 6]) #Calculate correlations 
}
print(correlations) #Print results 0.22691526 -0.09273544  0.09493204  0.06363531 -0.04648270

#As the results above, X1, X2, X3, and X4 has strong relationship, so they are selected to predict.
#Transform data
log_data <- cbind(my.data[,1], my.data[,2], my.data[,3], my.data[,4], my.data[,6]) 

#The transformation of six columns to reduce skewness
for (i in 1:5) { 
  if (i == 2) { #If it is X2 (X2 has a negative skew)
    log_data[, i] <- (log_data[, i])^3 #Apply cube for it
  } else { #Others have a positive skew
    log_data[, i] <- log(log_data[, i]) #Apply log transformation for them
  }
}

#After transforming data, I use Min-Max normalisation to make them be on the same scale [0,1]
#Min max transformation
nor_data <- log_data 
for(i in 1:5) {
  nor_data[, i] <- (nor_data[,i]-min(nor_data[,i]))/(max(nor_data[,i])-min(nor_data[,i]))
}

print(cor(nor_data[,2], nor_data[,5]))
#As X2 has a negative correlation with Y, I apply negation function for it
nor_data[,2] <- 1 - nor_data[,2] #1 is a max value from (0,1)

# Use for loop to draw histograms for min_max data
for (i in 1:5) { 
  hist(nor_data[, i], #Draw histogram for each column
       main = if(i <= 4) { #Title of four features
         sprintf("Histogram of X%d", i) 
       } else { #Title of Y
         sprintf("Histogram of Y") 
       },
       xlab = if(i <= 4) { #Name of four features
         sprintf("X%d", i) 
       } else { #Name of Y
         sprintf("Y") 
       })
}

#Save data
new_data <- nor_data
write.table(new_data,"VanAnDuong-transformed.txt")




#Task 4
#Choose OWA as it has a lowest RMSE
weight <- c(0.800547752843707, 0.0895834648215261, 0.109868782334779, 0)
#Assign input and expected output
Y <- 60 #Expected output
X <- c(19.1, 43.29, 19.7, 43.4) #Input

#Transform input
for (i in 1:4) { 
  if (i == 2) { #If it is X2
    X[i] <- (X[i])^3 #Apply cube for it
  } else { #Others 
    X[i] <- log(X[i]) #Apply log transformation for them
  }
}
#Min max
for(i in 1:4) {
  X[i] <- (X[i]-min(log_data[,1]))/(max(log_data[,i])-min(log_data[,i]))
}
#Negation X2
X[2] <- 1 - X[2] #1 is a max value from (0,1)

#Use OWA function to predict
Y_pre <- OWA(X,weight) #0.298907807416299

#Transfer predicted Y to the same scale with expected Y
Y_scale = min(log_data[,5]) + (max(log_data[,5]) - min(log_data[,5])) * Y_pre

Y_predicted<-exp(Y_scale) #61.62
