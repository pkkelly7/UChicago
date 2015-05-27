
#import the swap-a-lease data

car_data <- read.csv("C:/Users/Patrick/Documents/Education/UChicago/Leadership_Management_1/transfer_data_new.csv",
         header = TRUE)

summary(car_data)

head(car_data)

#checking 'Make'
table(car_data$Make)
missing()

#limiting to only Make == "BMW"
BMW_data <- car_data[car_data$Make == "BMW",]
summary(BMW_data)

#CLEANING DATA
summary(BMW_data)

BMW_data[BMW_data$Payment <= 0,1:10]
BMW_data[BMW_data$Payment < 100,1:10]
BMW_data[BMW_data$Payment > 2000,1:10]
BMW_data <- BMW_data[BMW_data$Payment > 95 & BMW_data$Payment < 9999,]
#BMW_data <- subset(BMW_data, Payment > 95 & Payment < 9999)
plot(BMW_data$Payment)

hist(BMW_data$months.on.site, breaks = 51)
BMW_data[BMW_data$months.on.site < 0,]
BMW_data <- BMW_data[BMW_data$months.on.site >= 0 & BMW_data$months.on.site <= 50,]

table(BMW_data$mo.left)
hist(BMW_data$mo.left, breaks = 105)
BMW_data <- BMW_data[BMW_data$mo.left > 0 & BMW_data$mo.left < 120,]

BMW_data[BMW_data$current.mi > 100000,]

check_models <- data.frame(table(BMW_data$Model))
check_models <- check_models[check_models$Freq > 0,]
check_models

check_style <- data.frame(table(BMW_data$Style))
check_style <- check_style[check_style$Freq > 0,]
check_style

write.csv(BMW_data,file="C:/Users/Patrick/Documents/Education/UChicago/Leadership_Management_1/transfer_data_BMW.csv")
