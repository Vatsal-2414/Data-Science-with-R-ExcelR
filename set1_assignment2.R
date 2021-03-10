data <- read.csv('/Users/vatsalmandalia/Set1_assignment2.csv')
View(data)
attach(data)
plot(Name.of.company,Measure.X)
?plot
library(ggplot2)
ggplot(data, aes(Name.of.company,Measure.X))+geom_line(data= data, aes(Name.of.company,Measure.X))
class(data)
