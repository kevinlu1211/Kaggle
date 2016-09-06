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
str(hp2001Q1)

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

args(geom_histogram)
args(stat_bin)
p1 <- ggplot(housing, aes(x=Home.Value))
p1 + geom_histogram()
# Note here that binwidth is the range of each bin so with binwidth = 4000 each bin will only have values
# from [x, x+4000], as our Home.Value data ranges from 0 to 750000 increasing binwidth to 4000 makes it clearer
# than the default bins = 30 (which means that only 30 columns are plotted)
p1 + geom_histogram(binwidth = 4000)
p1 + geom_histogram(bins = 100)

housing.sum <- aggregate(housing["Home.Value"], housing["State"], FUN = mean)
housing.sum <- aggregate(Home.Value ~ State, data = housing, FUN = mean) # these are equivalent 

# This won't work as a bar plot summarizes the data, but we have already done that in our housing.sum as it is the average of all house prices
# and the default behaviour for geom_bar() is to count the number of "things" in each feature (or state in our case)
p2 <- ggplot(housing.sum, aes(x=State, y = Home.Value))
p2 + geom_bar() # Error: stat_count() must not be used with a y aesthetic.

# Hence to fix this we must use the stat = "identity" parameter
p2 + geom_bar(stat="identity")

### EXERCISE 2 ###
p1 <- ggplot(dat, aes(x = CPI, y = HDI))
p1 + geom_point() + geom_smooth(method = "lm")
p1 + geom_point() + geom_smooth()
p1 + geom_point() + geom_smooth(span = .4)

### SCALES ###
p3 <- ggplot(housing,
             aes(x = State,
                 y = Home.Price.Index)) + 
  theme(legend.position="top",
        axis.text=element_text(size = 6))

# Note here that the legend isn't very informative because it shows 5 digits (due to the quarters)
(p4 <- p3 + geom_point(aes(color = Date),
                       alpha = 0.5,
                       size = 1.5,
                       position = position_jitter(width = 0.25, height = 0)))

# Now we modify the breaks and labels for the x axis and color scales
p4 + 
  # first modify the label of the x axis
  scale_x_discrete(name ="State Abbreviation") +
  # now modify the legend
  scale_color_continuous(
                         # first set the name of the legend
                         name="",
                         # next identify what points in the data we want to replace the break points with
                         breaks = c(19751, 19941, 20131),
                         # now set what we want to replace these break points by
                         labels = c(1971, 1994, 2013),
                         # set the colors for high and low
                         low = "blue", high = "red")


p4 +
  scale_color_gradient2(name="",
                        breaks = c(19751, 19941, 20131),
                        labels = c(1971, 1994, 2013),
                        low = "blue",
                        high = "red",
                        mid = "green",
                        midpoint = 19941)

### Exercise 3 ###
p1 <- ggplot(dat, aes(x=CPI, y=HDI))
p2 <- p1 + geom_point(aes(color=Region))
p3 <- p2 +  
      scale_x_continuous(name = "Corruption Perception Index") +
      scale_y_continuous(name = "Human Development Index")
p3
p4 <- p3 +
      scale_color_manual(name = "Region of the world",
                         values = c("#24576D",
                                    "#099DD7",
                                    "#28AADC",
                                    "#248E84",
                                    "#F2583F",
                                    "#96503F"))
p4

### FACETING ###

# The purpose of faceting is to create separate graphs for subsets of data

# Note how there are too many lines in the graph for the graph to be useful
p1 <- ggplot(housing, aes(x=Date, y = Home.Value))
p1 + geom_line(aes(color=State))

# use faceting
p2 <- p1 + geom_line() + facet_wrap(~State, ncol=10)
p3 <- p1 + geom_line() + facet_grid(~State) # What does this do?

### THEMES ###
p2 + theme_linedraw()
p2 + theme_minimal() + theme(text = element_text(color="turquoise"))

# Creating a new theme
theme_new <- theme_bw() +
  theme(plot.background = element_rect(size = 1, color = "blue", fill = "green"),
        text=element_text(size = 12, color = "ivory"),
        axis.text.y = element_text(colour = "purple"),
        axis.text.x = element_text(colour = "red"),
        panel.background = element_rect(fill = "pink"),
        strip.background = element_rect(fill = "orange"))

p2 + theme_new

### FAQ ###

# To plot two variables in a single graph as separate points, with different color depending on which variable it is
housing.byyear <- aggregate(cbind(Home.Value, Land.Value) ~ Date, data = housing, mean)

# The wrong way
head(housing.byyear)
ggplot(housing.byyear,
       aes(x=Date)) +
  geom_line(aes(y=Home.Value), color="red") +
  geom_line(aes(y=Land.Value), color="blue")

# The right way
library(tidyr)

# use the gather function in tidyr what this function does is:
# that it gathers multiple columns of data and collapses it into a single
# column type which each type being a column, the variables that need to be
# duplicated as a result of collapsing the table are automatically duplicated

# So this function call, gets the house.byyear data frame, and then names the
# columns for home.land.byyear through the value, and key, then collapses the 
# Home.Value and Land.Value into a single column
home.land.byyear <- gather(housing.byyear,
                           value = "value",
                           key = "type",
                           Home.Value, Land.Value)
head(home.land.byyear)

### PUTTING IT ALL TOGETHER ###
dat <- read.csv("dataSets/EconomistData.csv")
str(dat)
p1 <- ggplot(dat, aes(x=CPI, y=HDI))
p2 <- p1 + geom_smooth(aes(group=1), 
                       se = FALSE, 
                       method = "lm", 
                       formula = y ~ log(x),
                       color="red")

p3 <- p2 + 
  geom_point(shape = 1, size = 4, aes(color=Region)) + 
  geom_point(shape = 1, size = 4.5, aes(color=Region)) + 
  geom_point(shape = 1, size = 5, aes(color=Region))


pointsToLabel <- c("Russia", "Venezuela", "Iraq", "Myanmar", "Sudan",
                   "Afghanistan", "Congo", "Greece", "Argentina", "Brazil",
                   "India", "Italy", "China", "South Africa", "Spane",
                   "Botswana", "Cape Verde", "Bhutan", "Rwanda", "France",
                   "United States", "Germany", "Britain", "Barbados", "Norway", "Japan",
                   "New Zealand", "Singapore")

library(ggrepel)
?ggrepel
p4 <- p3 + geom_text_repel(aes(label=Country),
                     color = "gray20",
                     data = subset(dat, Country %in% pointsToLabel),
                     force = 10)
p4

# converting the region into factors
dat$Region <- factor(dat$Region,
                     levels = c("EU W. Europe",
                                "Americas",
                                "Asia Pacific",
                                "East EU Cemt Asia",
                                "MENA",
                                "SSA"),
                     labels = c("OECD",
                                "Americas",
                                "Asia &\nOceania",
                                "Central &\nEastern Europe",
                                "Middle East &\nnorth Africa",
                                "Sub-Saharan\nAfrica"))

# access the data object that ggplot has
p4$data <- dat
p5 <- p4 +
  scale_x_continuous(name = "Corruption Perceptions Index, 2011 (10=least corrupt)",
                     limits = c(.9, 10.5),
                     breaks = 1:10) +
  scale_y_continuous(name = "Human Development Index, 2011 (1=Best)",
                     limits = c(0.2, 1.0),
                     breaks = seq(0.2, 1.0, by = 0.1)) +
  scale_color_manual(name = "",
                     values = c("#24576D",
                                "#099DD7",
                                "#28AADC",
                                "#248E84",
                                "#F2583F",
                                "#96503F")) +
  ggtitle("Corruption and Human development")

library(grid) # for the 'unit' function
p6 <- p5 +
   theme_minimal() + # start with a minimal theme and add what we need
   theme(text = element_text(color = "gray20"),
         legend.position = c("top"), # position the legend in the upper left 
         legend.direction = "horizontal",
         legend.justification = 0.1, # anchor point for legend.position.
         legend.text = element_text(size = 11, color = "gray10"),
         axis.text = element_text(face = "italic"),
         axis.title.x = element_text(vjust = -1), # move title away from axis
         axis.title.y = element_text(vjust = 2), # move away for axis
         axis.ticks.y = element_blank(), # element_blank() is how we remove elements
         axis.line = element_line(color = "gray40", size = 0.5),
         axis.line.y = element_blank(),
         panel.grid.major = element_line(color = "gray50", size = 0.5),
         panel.grid.major.x = element_blank()
   )
p6

# get the r squared
mR2 <- summary(lm(HDI ~ log(CPI), data = dat))$r.squared

png(file = "images/econScatter10.png", width = 800, height = 600)
p6
grid.text("Sources: Transparency International; UN Human Development Report",
          x = .02, y = .03,
          just = "left",
          draw = TRUE)
grid.segments(x0 = 0.81, x1 = 0.825,
              y0 = 0.90, y1 = 0.90,
              gp = gpar(col = "red"),
              draw = TRUE)
grid.text(paste0("R² = ",
                 as.integer(mR2*100),
                 "%"),
          x = 0.835, y = 0.90,
          gp = gpar(col = "gray20"),
          draw = TRUE,
          just = "left")

dev.off()
p6
