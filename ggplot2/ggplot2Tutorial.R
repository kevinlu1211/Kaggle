### GGPLOT2 TUTORIAL ###
setwd("/Users/kevinlu/Documents/Data Science/Kaggle/ggplot2")
housing <- read.csv("dataSets/landdata-states.csv")
str(housing)

### INTRODUCTION ###

# using default package to plot a histogram
hist(housing$Home.Value)

# now use ggplot2
library(ggplot2)
ggplot(housing, aes(x = Home.Value)) + geom_histogram()

# using default package for color scatter plot
plot(Home.Value ~ Date, data = subset(housing, State == "MA"))
points(Home.Value ~ Date, col = "red", data = subset(housing, State == "TX"))
# pch changes the icon for the legend
legend(19750, 400000, c("MA", "TX"), title = "State", col = c("black", "red"), pch=c(1,1)) 

# now use ggplot2
ggplot(subset(housing, State %in% c("MA", "TX")), aes(x=Date, y = Home.Value, color = State)) + geom_point()

### GEOMETRIC OBJECTS AND AESTHETICS ###

### Points (Scatterplot) ###

# use ggplot2 to plot the 2001 q1 housing data
hp2001Q1 <- subset(housing, Date == 20011)
ggplot(hp2001Q1, aes(y=Structure.Cost, x = Land.Value)) + geom_point()

# create a new column in the data frame of hp2001Q1
hp2001Q1$pred.SC <- predict(lm(Structure.Cost ~ Land.Value, data = hp2001Q1))

### Lines (Prediction Line) ###

p1 <- ggplot(hp2001Q1, aes(x=Land.Value, y=Structure.Cost))
p1 + geom_point(aes(color=Home.Value)) + geom_line(aes(y=pred.SC))

# geom_smooth() is predicted value of y given x
p1 + geom_point(aes(color=Home.Value)) + geom_smooth()

# Note here that if only geom_line() is used it will connect the points of geom_point
p2 <- ggplot(hp2001Q1, aes(x=Land.Value, y=Home.Value))
p2 + geom_point(size = 3) + geom_line()

# to make a prediction line do geom_line(aes(y=...))
p2 + geom_point(size = 3) + geom_line(aes(y = 1))

# note that size = 1 is needed here as ggplot defaults to size = 3
p2 + geom_point(size = 3) + geom_line(aes(y=pred.SC), size = 1) 

### Text (Label Points) ###
library(ggrepel)
p1 + geom_text(aes(label=State), size = 3)
p1 + 
  geom_point() + 
  geom_text_repel(aes(label=State), size = 3)

### Aesthetic Mapping VS Assignment ###

# Note that aesthetics that are specific to the geom should be in geom_...(aes(size = 3)), instead of being inside the ggplot function
p2 <- ggplot(hp2001Q1, aes(x=Land.Value, y=Structure.Cost, size = 3))
# note that ggplot(hp2001Q1, aes(x=Land.Value, y=Structure.Cost), size = 3) would have no effect as well
p2 + geom_point(aes(color=Home.Value)) # note how we have an extra icon in the legend from the size = 3 in p2

# Correct way add the size specific to each geom outside of the ggplot and in the geom_... function
p3 <- ggplot(hp2001Q1, aes(x=Land.Value, y=Structure.Cost))
p3 + geom_point(color = "red", size = 10) # this is the correct way

# we could even give them size and color according to their features
p3 + geom_point(aes(size=Land.Value, color = Structure.Cost))
p1 + geom_point(aes(color=Home.Value, shape = region))

### Exercise 1 ###

dat <- read.csv("dataSets/EconomistData.csv")
str(dat)
## Create a scatter plot with CPI on x axis and HDI on the y axis
# p1 + geom_point() + geom_text(aes(label=Country))
p1 <- ggplot(dat, aes(x=CPI, y=HDI))

# making the scatter plot have blue points
p1 + geom_point(color="blue")

# plotting the points by region
p1 + geom_point(aes(color=Region))

# plotting the boxplots of CPI by region
p2 <- ggplot(dat, aes(x=Region, y = CPI))
p2 + geom_boxplot(aes(color=Region))

# Overlay the boxplot with the points
p2 + geom_boxplot(aes(color=Region)) + geom_point(aes(color=Region))

### STATISTICAL TRANSFORMATIONS ###




