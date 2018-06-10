#________________________Nile_Project________________________
#Source_Code

#Step 1. Descriptive Statistics
#install.packages(psych)
library(psych)
describe(Nile)

#a. Measures of central tendency:
#I. Mode
get_mode<- function(x)
{
  duplicate<- table(x)
  names(duplicate)[duplicate==max(duplicate)]
}
get_mode(Nile)
#II. Median 
median(Nile)
#III. Mean
mean(Nile)

#b. Measures of dispersion (variability):
#I. Variance
variance<-function(x)
{
  s<-sum((x-mean(x))^2)
  s/(length(x)-1)
}

variance(Nile)
#II. Standard Deviation
Standard_Deviation<-function(x)
{
  sqrt(variance(x))
}

Standard_Deviation(Nile)
#III. Range
max(Nile)-min(Nile)
#IV. Quartile and Inter quartile Range (IQR)
data.frame(quantile(Nile))
IQR(Nile)
#V. Box-whisker Plot
boxplot(Nile)
#VI. Z-Score
plot(density(Nile))

#c. Graphing data “describing data using graph”:
#I. Dot-Plot
dotchart(as.numeric(Nile))
#II. Bar Graph 
barplot(table(Nile))
#III. Pie Chart 
pie(Nile)
#IV. Histogram
hist(Nile)


#Step 2. Correlations

plot(c(1:100),Nile)

cor(c(1:100), Nile, use="complete.obs", method="pearson")


#Step 3. Inferential Statistics

#a. Confidence Interval of population mean:
t.test(Nile)

#b. Test of hypotheses:
t.test(Nile, mu = 925, alternative = "greater", paired = FALSE)

#c. Correlation Test:
cor.test(c(1:100), Nile)