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

#Transform input
log_data[,1] <- log(log_data[,1])
log_data[,2] <- (log_data[,2])^3
log_data[,3] <- log(log_data[,3])
log_data[,4] <- log(log_data[,4])
log_data[,5] <- log(log_data[,5])

nor_data <- log_data 

nor_data[,1] <- (nor_data[,1]-min(nor_data[,1]))/(max(nor_data[,1])-min(nor_data[,1])) 
nor_data[,2] <- (nor_data[,2]-min(nor_data[,2]))/(max(nor_data[,2])-min(nor_data[,2]))   
nor_data[,3] <- (nor_data[,3]-min(nor_data[,3]))/(max(nor_data[,3])-min(nor_data[,3])) 
nor_data[,4] <- (nor_data[,4]-min(nor_data[,4]))/(max(nor_data[,4])-min(nor_data[,4])) 
nor_data[,5] <- (nor_data[,5]-min(nor_data[,5]))/(max(nor_data[,5])-min(nor_data[,5]))

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


#Task 3
#Load source package 
source("AggWaFit718.R")
#WAM
fit.QAM(new_data[,c(1:4,5)],"WAMoutput.txt","WAMstats.txt")
#WPM p= 0.5
fit.QAM(new_data[,c(1:4,5)], "PMoutput.txt", "PMstats.txt",g=PM05,g.inv = invPM05)
#WPM p= 2
fit.QAM(new_data[,c(1:4,5)],"QMoutput.txt", "QMstats.txt",g=QM,g.inv = invQM)
#OWA
fit.OWA(new_data[,c(1:4,5)],"OWAoutput1.txt","OWAstats1.txt")

#Task 4
#Choose OWA as it has a lowest RMSE
weight <- c(0.800547752843707, 0.0895834648215261, 0.109868782334779, 0)
#Assign input and expected output
Y <- 60 #Expected output
X <- c(19.1, 43.29, 19.7, 43.4) #Input

#Transform input
X[1] <- log(X[1])
X[2] <- (X[2])^3
X[3] <- log(X[3])
X[4] <- log(X[4])

X[1] <- (X[1]-min(log_data[,1]))/(max(log_data[,1])-min(log_data[,1])) 
X[2] <- (X[2]-min(log_data[,2]))/(max(log_data[,2])-min(log_data[,2]))   
X[3] <- (X[3]-min(log_data[,3]))/(max(log_data[,3])-min(log_data[,3])) 
X[4] <- (X[4]-min(log_data[,4]))/(max(log_data[,4])-min(log_data[,4])) 

#Negation X2
X[2] <- 1 - X[2] #1 is a max value from (0,1)

#Use OWA function to predict
Y_pre <- OWA(X,weight) #0.298907807416299

#Transfer predicted Y to the same scale with expected Y
Y_scale = min(log_data[,5]) + (max(log_data[,5]) - min(log_data[,5])) * Y_pre

Y_predicted<-exp(Y_scale) #65.71


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

weight1 <- c(0.849157223623278, 0.150842776376723, 0, 0)
weight2 <- c(0.79798950194464, 0.20201049805536, 0, 0)
weitht3 <- c(0.927624710119346, 0.0723752898806525, 0, 0)

#Use WAM function to predict
Y_pre_wam <- QAM(X,weight1) 

Y_scale_wam = min(log_data[,5]) + (max(log_data[,5]) - min(log_data[,5])) * Y_pre

Y_predicted_wam <-exp(Y_scale_wam) 


#Use PM05 function to predict
Y_pre_PM05 <- QAM(X,weight2,g=PM05,g.inv = invPM05) 

Y_scale_PM05 = min(log_data[,5]) + (max(log_data[,5]) - min(log_data[,5])) * Y_pre

Y_predicted_PM05 <-exp(Y_scale_PM05) 


#Use QAM function to predict
Y_pre_QAM <- QM(X,weight3,g=QM,g.inv = invQM) 

Y_scale_QAM = min(log_data[,5]) + (max(log_data[,5]) - min(log_data[,5])) * Y_pre

Y_predicted_QAM <-exp(Y_scale_QAM)

rm(Y_predicted_wam, Y_scale_wam)


citation(AggWaFit718")

