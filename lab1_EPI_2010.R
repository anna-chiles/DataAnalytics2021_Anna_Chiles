EPI_data <- read.csv("C:/Users/315ac/OneDrive/Documents/School Stuff/Grad School/ITWS 4600 - Data Analytics/2010EPI_data.csv")
View(EPI_data)
attach(EPI_data)
fix(EPI_data)
EPI #error in finding value for EPI, needs debugging
tf <- is.na(EPI)
E <- EPI[!tf]
summary(EPI_data)
fivenum(EPI_data,na.rm=TRUE)#causes error, needs debugging
stem(EPI_data)
help(hist)
hist(EPI_data)
hist(EPI_data, seq(30., 95., 1.0), prob=TRUE)
help(lines)
lines(density(EPI_data,na.rm=TRUE,bw=1.))
help(rug)
rug(EPI_data)