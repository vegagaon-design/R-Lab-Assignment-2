######################## CE 372 LAB 3: Descriptive statistics and data visualization ###############################
### Name: Marisol Vega-Gaona
### PID: 181474617

#Question 1:
#Step 1: Clear console and environment
## Remove variables from Environment ##
rm(list=ls())   

#Question 1:
#Step 1: Clear console and environment
## Clear console (ctrl+L)##
cat("\f") 

#Question 1:
#Step 1: Clear console and environment
###### Close all open devices (plots) #######
if(!is.null(dev.list())) dev.off()

start.time = Sys.time() 

#Question 1:
## get the current working directly ##

## Prints the current working directory.
#Step 2: set up your directory and Read your file
getwd() 


## to set a directory to current working directly ##
## Changes the working directory to the folder where your dataset is stored.
setwd("C:/Users/vegagaon/Downloads/CE372_Assignment_2") 


## read data from an excel file ##
## Reads a CSV file into a data frame called my_data.header=T → first row contains column names.

climate=read.csv("CE 372_dataset5.csv",
                 header=T,sep=",")

my_data=read.csv("CE 372_dataset6.csv",
                  header=T,sep=",")

######################## QUESTION 1 ###############################
#Question 1:

names(my_data) #noting the correct names for indexing for data set 6 

names(climate) #noting the correct names for indexing for data set 5

# Step 3: Create an index for Condition 1: station=="MERRA" & Condition 2: year==2001 & Condition 3: factor=="Temperature" (ALL 3 conditions must be true at the same time!) 
# Step 4: Using this index extract column average

idx_merra_2001_temp <- my_data$Station == "MERRA" &
  my_data$Year    == 2001 &
  my_data$Factor  == "Temperature"

merra_2001_temp_vals <- my_data$MonthlyAverage[idx_merra_2001_temp]


#Step 5: Find the mean of the extracted column
## Mean of a dataset ##
merra_2001_temp_mean <- mean(merra_2001_temp_vals, na.rm = TRUE)
print(merra_2001_temp_mean)



#Step 6: Repeat the steps for dataset "climate" and "my_data" 
## NARR Temperature 2001
idx_narr_2001_temp <- my_data$Station == "NARR" &
  my_data$Year    == 2001 &
  my_data$Factor  == "Temperature"

narr_2001_temp_vals <- my_data$MonthlyAverage[idx_narr_2001_temp]
narr_2001_temp_mean <- mean(narr_2001_temp_vals, na.rm = TRUE)
print(narr_2001_temp_mean)

## MERRA Sunshine 2001
idx_merra_2001_sun <- my_data$Station == "MERRA" &
  my_data$Year    == 2001 &
  my_data$Factor  == "Sunshine"

merra_2001_sun_vals <- my_data$MonthlyAverage[idx_merra_2001_sun]
merra_2001_sun_mean <- mean(merra_2001_sun_vals, na.rm = TRUE)
print(merra_2001_sun_mean)

## NARR Sunshine 2001
idx_narr_2001_sun <- my_data$Station == "NARR" &
  my_data$Year    == 2001 &
  my_data$Factor  == "Sunshine"

narr_2001_sun_vals <- my_data$MonthlyAverage[idx_narr_2001_sun]
narr_2001_sun_mean <- mean(narr_2001_sun_vals, na.rm = TRUE)
print(narr_2001_sun_mean)

########## END OF QUESTION 1 ########################


########### QUESTION 2 #####################
## Shows min, max, mean, quartiles for each column.
summary(my_data)
print(my_data)
#Step 3: Create an index for station=="MERRA" & factor=="Temperature"
idx_merra_temp <- my_data$Station == "MERRA" &
  my_data$Factor  == "Temperature"

#Step 4: Using this index extract column MonthlyAverage
merra_temp_vals <- my_data$MonthlyAverage[idx_merra_temp]

#Step 5: Plot the extracted column with months (1:24)
months <- 1:24   # Assignment requires months 1–24

plot(x = months, y = merra_temp_vals, type = "b",
     main = "Average Monthly Temperature (MERRA vs NARR)",
     xlab = "Month (1–24)",
     ylab = "Temperature (°F)",
     col = "blue",
     xlim = c(1, 24),
     ylim = c(min(merra_temp_vals), max(merra_temp_vals)))

#Step 6: Overlay NARR Temperature using par(new=TRUE)
idx_narr_temp <- my_data$Station == "NARR" &
  my_data$Factor  == "Temperature"

narr_temp_vals <- my_data$MonthlyAverage[idx_narr_temp]

par(new = TRUE)   # Overlay on previous plot

plot(x = months, y = narr_temp_vals, type = "b",
     col = "red",
     axes = FALSE,   # No new axes
     xlab = "",      # Blank labels as required
     ylab = "",
     xlim = c(1, 24),
     ylim = c(min(merra_temp_vals, narr_temp_vals),
              max(merra_temp_vals, narr_temp_vals)))

legend("topleft",
       legend = c("MERRA", "NARR"),
       col = c("blue", "red"),
       pch = 1,
       bty = "o")

######################## SUNSHINE PLOT ###############################

#Step 7: Repeat the same steps for Sunshine

#Index for MERRA Sunshine
idx_merra_sun <- my_data$Station == "MERRA" &
  my_data$Factor  == "Sunshine"

merra_sun_vals <- my_data$MonthlyAverage[idx_merra_sun]

#Plot MERRA Sunshine
plot(x = months, y = merra_sun_vals, type = "b",
     main = "Average Monthly Sunshine (MERRA vs NARR)",
     xlab = "Month (1–24)",
     ylab = "Sunshine (%)",
     col = "blue",
     xlim = c(1, 24),
     ylim = c(min(merra_sun_vals), max(merra_sun_vals)))

#Overlay NARR Sunshine
idx_narr_sun <- my_data$Station == "NARR" &
  my_data$Factor  == "Sunshine"

narr_sun_vals <- my_data$MonthlyAverage[idx_narr_sun]

par(new = TRUE)

plot(x = months, y = narr_sun_vals, type = "b",
     col = "red",
     axes = FALSE,
     xlab = "",
     ylab = "",
     xlim = c(1, 24),
     ylim = c(min(merra_sun_vals, narr_sun_vals),
              max(merra_sun_vals, narr_sun_vals)))

legend("topleft",
       legend = c("MERRA", "NARR"),
       col = c("blue", "red"),
       pch = 1,
       bty = "o")

########## END OF QUESTION 2 ########################

########### QUESTION 3 ################################# 
## Use any statistical value/parameter or plots to determine your answer.
## 3) (2 pts) Using any statistical parameter and/or data visualization, recommend which climatic
#station (MERRA or NARR) is better. Explain why.

## Temperature Seasonal Variation
## Both stations show clear seasonal cycles.

## Temperatures increase during mid‑year months, that's Summer. And, decrease during early and late months (Winter).

## NARR and MERRA may differ slightly in peak temperature and variability.


## Sunshine Seasonal Variation
## Sunshine percentages rise during summer months and fall during winter months.

## MERRA and NARR may differ in amplitude (how strong the seasonal cycle is).

## Sunshine typically peaks around months 6–9 and dips around months 1–3 and 11–12.

######### END OF QUESTION 3 ##################






