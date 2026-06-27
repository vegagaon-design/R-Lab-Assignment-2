######################## CE 372 LAB 3: Descriptive statistics and data visualization ###############################

## Remove variables from Environment ##
rm(list=ls())   

## Clear console (ctrl+L)##
cat("\f") 

###### Close all open devices (plots) #######
if(!is.null(dev.list())) dev.off()

start.time = Sys.time() 

## get the current working directly ##
getwd() 


## to set a directory to current working directly ##
setwd("C:/Users/rahul/OneDrive - Michigan State University/Courses/TA/4_CE 372 Summer Semester 2022/R-Lab/Week #5 and #6") 


## read data from an excel file ##
my_data=read.csv("CE 372_dataset5.csv",
                  header=T,sep=",")

## Mean of a dataset ##
average1=mean(my_data$IRI,na.rm=T)
print(average1)

summary(my_data)

xx=c(-21,-5,2,3,4.2,7,8,12,18,54)
yy=mean(xx,trim=0.5,na.rm=T)
yy

## Median of a dataset ##
median1=median(my_data$IRI,na.rm=T)
median1

## Standard deviation, variance and Coefficient of variation ##
Std_Dev = sd(my_data$IRI,na.rm=T)
Std_Dev

Variance = var(my_data$IRI,na.rm=T)
Variance

print(Std_Dev^2)
print(sqrt(Variance))

Coeff_of_Variation = Std_Dev/average1
Coeff_of_Variation

## Covariance and Correlation ##
Covariance = cov(my_data$IRI,my_data$TRCrack) #,use="na.or.complete")
print(paste("Covariance =",Covariance),quote=F)

Correlation = cor(my_data$IRI,my_data$TRCrack,use="na.or.complete")
print(paste("Correlation =",Correlation),quote=F)

## Plot graphs to visualize correlation ##
plot(my_data$IRI,my_data$Fault)

pairs(my_data[,2:4]) ## compare all pairwise in a single plot

##### Plot correlation plots ####

install.packages("corrplot")

library(corrplot)

# Positive correlations are shown in blue, negative correlations in red

# Color intensisty and size of the circle are proportional to the correlation coefficients

corrplot(cor(my_data[,2:4], method="spearman"), method ="circle", order = "alphabet", type = "upper") 

corrplot(cor(my_data[,2:4], method="spearman"), method ="number", type = "upper")

## Estimate percentiles ##
my_percentile = quantile(my_data$IRI, probs=c(0,0.25,0.5,0.9,1),na.rm=T)
my_percentile

## Scatter plot ##

plot(x=my_data$AgeMonth,y=my_data$IRI,type="p",
     main = "IRI vs Age (Months)",
     xlab = "Age (Months)",
     ylab = "IRI (in/mile)",
     xlim = c(0,300),
     ylim = c(0,200),
)

##### Indexing of desired input #######
Idx1=my_data$BMP==0.898 & my_data$EMP==5.886

plot(x=my_data$AgeMonth[Idx1],y=my_data$IRI[Idx1],type="b",
     main = "IRI vs Age (Months)",
     xlab = "Age (Months)",
     ylab = "IRI",
     xlim = c(0,300),
     ylim = c(0,200),
)

## Saving a plot ##

png("scatter_plot.png")   ## Open graphics device as png function
## You can use BMP, JPEG, pdf etc
plot(x=my_data$AgeMonth[Idx1],y=my_data$IRI[Idx1],type="b",
     main = "IRI vs Age (Months)",
     xlab = "Age (Months)",
     ylab = "IRI",
     xlim = c(0,300),
     ylim = c(0,200),
)
dev.off()                 ## Close the opened device

## Plotting multiple data series in single plot ##
Idx1=my_data$BMP==0.898 & my_data$EMP==5.886
Idx2=my_data$BMP==0.905 & my_data$EMP==5.602
Idx3=my_data$BMP==5.617 & my_data$EMP==6.503
par(mgp=c(3,1,0),mar=c(5,4,4,2)+0.1)  # Parameter to change axis label and tick spacing
plot(x=my_data$AgeMonth[Idx1],y=my_data$IRI[Idx1],type="b",
     col = 2,pch=2,     # colour and point character style
     main = "IRI vs Age",
     xlab = "Age (Months)",
     ylab = "IRI (in/mile)",
     xlim = c(0,300),
     ylim = c(0,250),
     #log='x',  # create x or y axis in log scale (try y and xy)
     las=0,    # label style (horizontal or vertical). Try changing to 1,2,3
     bty = "o",  # box type. Try changing to n
     cex.main=1, # Character expansion ratio for title (try changing less than
     # or greater than 1)
     cex.lab=1,  # Character expansion ratio for labels
     cex.axis=1,  # Character expansion ratio for axis text
)

axis(side=1,at=c(0,50,100,150,200,250,300))  # Fix tick labels on x and y axis
axis(side=2,at=c(0,50,100,150,200,250))  
abline(h=170,col="red",lty=5,lwd=2) # Make a line in the plot
text(280,30,"My text 1")            # Show text inside the plot
mtext("My text 2",side=3,at=250)          # Show text on a side/margin
par(new=TRUE)                         # to hold the previous chart
plot(x=my_data$AgeMonth[Idx2],y=my_data$IRI[Idx2],type="b", 
     axes=F, xlab="",ylab="",col = 3,pch=1)
par(new=TRUE)                         # to hold the previous chart
plot(x=my_data$AgeMonth[Idx3],y=my_data$IRI[Idx3],type="b", 
     axes=F, xlab="",ylab="",col = 4,pch=1)
legend(x="topleft",legend=c("Series1","Series2","Series3"),
       col=c(2,3,4),pch=c(2,1,1),bty="o",cex=0.8)
dev.off()

## Plots multiple sub-plots in one plot ##
par(mfrow=c(1,2))
plot(x=my_data$AgeMonth[Idx1],y=my_data$IRI[Idx1],type="b",
     col = 2,pch=2,     # color and point character style
     main = "IRI vs Age",
     xlab = "Age (Months)",
     ylab = "IRI (in/mile)",
     xlim = c(0,300),
     ylim = c(0,250),
     las=0,    # label style (horizontal or vertical). Try changing to 1,2,3
     bty = "o",  # box type. Try changing to n
     cex.main=1, # Character expansion ration for title (try 
     #changing it less than or greater than 1)
     cex.lab=1,  # Character expansion ration for labels
     cex.axis=1,  # Character expansion ration for axis text
)
plot(my_data$AgeMonth[Idx1],y=my_data$TRCrack[Idx1],type="b",
     main ="Transverse Cracking vs Age",
     xlab="Age (Months)",
     ylab="Transverse Cracking (%)",
     xlim = c(0,300),
     ylim = c(0,100),
     col = 3,pch=1)
dev.off()

## Plot histigrams ##

hist(my_data$Fault, 
     main = "Histogram of Faulting",
     xlab = "Faulting (in)",
     ylab = "No. of occurrences",
     col="red", border="black",
     breaks = seq(0, #min(my_data$Fault,na.rm=T), 
                  max(my_data$Fault,na.rm=T), length.out = 5))
box()

## Plot bar charts ##

barplot(my_data$TRCrack,col="red",xlab="Age (Months)",
        ylab="Transverse Crack (%)",
        main="Bar plot for Transverse Cracking",
        names.arg=my_data$AgeMonth)
box()

#barplot(my_data$TRCrack~my_data$IRI)

## Plot 2 bars in the same plot ##
Idx4=my_data$DIR=="I" & my_data$AgeMonth>48
Idx5=my_data$DIR=="D"
bardata=rbind(my_data$IRI[Idx4],my_data$IRI[Idx5])
barplot(bardata,beside=T,col=c("red","green"),xlab="Index (Months)",
        ylab="IRI",names.arg=1:9,
        legend.text = c("IRI-I","IRI-D"),ylim=c(0,250),
        args.legend=list(x="topleft",cex=0.8))
box()

## Plot overlapping bars ##

barplot(my_data$IRI[Idx4],col="red",xlab="Index (Months)",ylim=c(0,250),
        ylab="IRI",names.arg=1:9)

par(new=T)
barplot(my_data$IRI[Idx5],col="green",axes=F,ylim=c(0,250))
legend(x="topleft",c("IRI-I","IRI-D"),col=c("red","green"),
       pch=c(15,15),bty="o",cex=1)
box()

## Box plot ##

boxplot(my_data$TRCrack,
        main="Boxplot for Transverse Cracking",
        ylab="Transverse Cracking (%)",col="red",notch=F)

boxplot(my_data$TRCrack~my_data$BMP,
        main="Boxplot for Transverse Cracking",
        ylab="Transverse Cracking (%)",xlab="Direction")

end.time = Sys.time()
time.taken = round(end.time - start.time,3)
time.taken

