source("GE_Data_Modification.r")

Data <- GE_Data_Modification()
Data.mean   <- apply(Data, 2, mean)
Data.sd <- apply(Data, 2, sd)
Data.names <- colnames(Data)
path.name = "D:/Work/NU/Master/Visualization/A2/GE.png"
png(path.name, width = 2000, height = 1000, res=200) 
x <- c(1:5)
layout(matrix(c(1,1,2,3),2,2, byrow = F))
plot(x, Data.mean, type = "b", lty = 5, pch = 25, 
     col = "red", xlab = "Gene", ylab = "Mean and Standard Dev",
     main = 'Mean and Standard Dv values',cex.main = 0.8)
lines(x, Data.sd, pch = 25, col = "blue", type = "b", lty = 2,)
legend(x= "topleft",legend=c("Mean", "Standard Dev"),
       col=c("red", "blue"), lty = 1:2,cex=0.7 ,inset = 0.2)

boxplot(Data.mean, Data.sd, names = c('Mean', 'Standard Dev'),
        xlab = 'Descriptor', ylab = 'Value', ylim = c(y.min, y.max),
        main = 'Distribution of the Mean and Standard Dev values')
plot(ecdf(Data.mean), xlab = 'Data Points', main =
       'Distribution of Mean', cex.main = 0.8,
     ylab = 'Fraction of data points',)

dev.off()  