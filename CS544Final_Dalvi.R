library(dplyr)

#preprocessing
data <- as.data.frame(read.csv("stroke_data.csv", header=T, sep=",", na.strings = c("", "NA")))
attach(data)
data$smoking_status <- as.character(data$smoking_status)
data$smoking_status <- ifelse(is.na(data$smoking_status), 
                             'No information available', data$smoking_status)


#Q1
#categorical 1
table(data$gender)
barplot(table(data$gender), col = "cyan", ylim = c(0,25000), las = 2, xlab = "Gender", ylab = "Stroke")
yes_stroke <- data[data$stroke == 1, ]
table(yes_stroke$gender)
barplot(table(yes_stroke$gender), col = "cyan", ylim = c(0,500), las = 2, xlab = "Gender", ylab = "Stroke")

#categorical 2
smoking_status <- table(data$smoking_status)
slice.labels <- names(smoking_status)
slice.percents <- round(smoking_status/sum(smoking_status)*100)
slice.labels <- paste(slice.labels, slice.percents)
slice.labels <- paste(slice.labels, "%", sep = "")
pie(smoking_status, labels = slice.labels, col = hcl(c(0, 60, 120))) 

#numerical 1
fivenum(data$age)
boxplot(data$age, horizontal = TRUE,  xaxt = "n", xlab = "Age of patients", col=hcl(1))
axis(side = 1, at=fivenum(data$age), labels = TRUE)
f <- fivenum(data$age)
f

#numerical 2
ggplot(data, aes(x=age)) + 
  geom_histogram(color="black", fill="lightblue") +   facet_grid(~stroke)


#Q2


#1
no_stroke <- data[data$stroke == 0, ]
A <- no_stroke$avg_glucose_level
B <- yes_stroke$avg_glucose_level
boxplot(A,B, xaxt = "n", xlab = "Whether patients have stroke", ylab = "Glucose levels of patients", col=c("red", "green"))

#2
dt=data.frame(Age=data$age,hypertension=data$hypertension,Stroke=data$stroke)
head(dt,10)
plot(dt , pch=20 , cex=1.5 , col="red")

#3
ggplot(data = data) + 
  geom_point(mapping = aes(x = age, y = heart_disease, colour = data$work_type ))

#4
p <- plot_ly(data, x = data$age, type="box", name = 'age')
p

q <-add_trace(p, x = data$bmi, type="box", name = 'bmi')
q  

w <-add_trace(q , x = data$avg_glucose_level,type = "box" , name ="glucose level" )
w

#Q3

#age
values <- data$age
tab <- table(values)

dframe <- as.data.frame(tab)
dframe

x <- as.numeric(as.character(dframe$values))

# probability distribution is 
f <- dframe$Freq / (sum(dframe$Freq))

# calculate  the  mean
mu  <- sum(x * f)
mu

# variance of the  distribution is 
sigmaSquare <-  sum((x -  mu)^2 * f)
sigmaSquare

sigma <- sqrt(sigmaSquare)
sigma

plot(x, f, type = 'h',  xlab = "Age of patients", ylab = "PMF", ylim = c(0, 0.04), main = "Spike plot for Age")
abline(h = 0 )

cdf <- c(0, cumsum(f))
cdfplot <- stepfun(x, cdf)
plot(cdfplot,  verticals=FALSE, pch=16, main="CDF Plot for Age", xlab = "Age", ylab = "CDF")

#bmi

data <- as.data.frame(read.csv("train_2v.csv", header=T, sep=","))
data
values <- data$bmi
tab <- table(values)

dframe <- as.data.frame(tab)
dframe

x <- as.numeric(as.character(dframe$values))

# probability distribution is 
f <- dframe$Freq / (sum(dframe$Freq))

# calculate  the  mean
mu  <- sum(x * f)
mu

# variance of the  distribution is 
sigmaSquare <-  sum((x -  mu)^2 * f)
sigmaSquare

sigma <- sqrt(sigmaSquare)
sigma

plot(x, f, type = 'h',  xlab = "BMI of patients", ylab = "PMF",ylim = c(0, 0.01), main = "Spike plot for BMI")
abline(h = 0 )

cdf <- c(0, cumsum(f))
cdfplot <- stepfun(x, cdf)
plot(cdfplot,  verticals=FALSE, pch=16, main="CDF Plot for BMI", xlab = "BMI", ylab = "CDF")

#Q4
#CLT
#age
age <-data$age

ctable <- table(age)
ctable
mu <- mean(age)
mu
sigma <- sd(age)
sigma


dframe <- as.data.frame(ctable)
dframe

x <- as.numeric(as.character(data$age))
x
hist(x, probability = TRUE, xlim = c(0, 100), xlab = "age", ylab = "Density", main = "Histogram of age")

# sample size 5
samples <- 1000
sample_size <- 5
xbar <- numeric(samples)
for(i in 1:samples){
  xbar[i] = mean(sample(x, size = sample_size, replace = T))
}
hist(xbar, prob = T, xlab = "Age", 
     main = "Densities of age with sample size 5", col = "blue")
mean1 <- mean(xbar)
sd1 <- sd(xbar)
mean1
sd1
#sample size 20
samples <- 1000
sample_size <- 20
xbar <- numeric(samples)
for(i in 1:samples){
  xbar[i] = mean(sample(x, size = sample_size, replace = T))
}
hist(xbar, prob = T, xlab = "Age", 
     main = "Densities of age with sample size 20", col = "red")
mean2 <- mean(xbar)
sd2 <- sd(xbar)
mean2
sd2

#sample size 50
samples <- 1000
sample_size <- 50
xbar <- numeric(samples)
for(i in 1:samples){
  xbar[i] = mean(sample(x, size = sample_size, replace = T))
}
hist(xbar, prob = T, xlab = "Age", 
     main = "Densities of age with sample size 50", col = "green")
mean3 <- mean(xbar)
sd3 <- sd(xbar)
mean3
sd3

cat("1st distribution:\nMean =",mean1,"\nSD =",sd1)
cat("2nd distribution:\nMean =",mean2,"\nSD =",sd2)
cat("3rd distribution:\nMean =",mean3,"\nSD =",sd3)

#glucose level
glucose <-data$avg_glucose_level  

ctable <- table(glucose)
ctable
mu <- mean(glucose)
mu
sigma <- sd(glucose)
sigma


dframe <- as.data.frame(ctable)
dframe

x <- as.numeric(as.character(data$avg_glucose_level))
x
max(data$avg_glucose_level)

hist(x, probability = TRUE, xlim = c(0,300 ), xlab = "Average Glucose Level", ylab = "Density", main = "Histogram of Average Glucose Level")
#sample size 5
samples <- 1000
sample_size <- 5
xbar <- numeric(samples)
for(i in 1:samples){
  xbar[i] = mean(sample(x, size = sample_size, replace = T))
}
hist(xbar, prob = T,xlim = c(0,200), xlab = "Avg Glucose level", 
     main = "Densities of Average Glucose Level with sample size 5", col = "blue")
mean1 <- mean(xbar)
sd1 <- sd(xbar)
mean1
sd1
#sample size 20
samples <- 1000
sample_size <- 20
xbar <- numeric(samples)
for(i in 1:samples){
  xbar[i] = mean(sample(x, size = sample_size, replace = T))
}
hist(xbar, prob = T,xlim = c(0,200), xlab = "Average Glucose Level", 
     main = "Densities of Average Glucose Level with sample size 20", col = "red")
mean2 <- mean(xbar)
sd2 <- sd(xbar)
mean2
sd2

#sample size 50
samples <- 1000
sample_size <- 50
xbar <- numeric(samples)
for(i in 1:samples){
  xbar[i] = mean(sample(x, size = sample_size, replace = T))
}
hist(xbar, prob = T,xlim = c(0,200), xlab = "Average Glucose Level", 
     main = "Densities of Average Glucose Level with sample size 20", col = "green")
mean3 <- mean(xbar)
sd3 <- sd(xbar)
mean3
sd3

cat("1st distribution:\nMean =",mean1,"\nSD =",sd1)
cat("2nd distribution:\nMean =",mean2,"\nSD =",sd2)
cat("3rd distribution:\nMean =",mean3,"\nSD =",sd3)

#Q5
#sampling 
library(sampling)
table(data$age)
hist(data$age)
mean_without_sampling <- mean(data$age)
sd(data$age)
#sample size = 500
# srswor
sample.size <- 500
s <- srswor(sample.size,nrow(data))
sample.1 <- data[s != 0, ]

mean_srswor <- mean(sample.1$age)
#srswr
set.seed(153)
s <- srswr(sample.size, nrow(data))
sample.2 <- data[s != 0, ]
mean_srswr <- mean(sample.2$age)

#Systematic Sampling
N <- nrow(data)
n <- 1000

k <- ceiling(N / n)
k

r <- sample(k, 1)
r

s <- seq(r, by = k, length = n)
s

sample.3 <- data[s, ]
table(sample.3$age)
mean_systematic <- mean(sample.3$age)

#Systematic Sampling with unequal probabilities
pik <- inclusionprobabilities(data$age, sample.size)
s <- UPsystematic(pik)
sample.4 <- data[s != 0, ]
table(sample.4$age)

data["age_range"] = NA

data$age_range <- cut(data$age, breaks = c(0, 25, 50, 75, Inf), labels = c('A', 'B', 'C', 'D'))

data_age <- data.frame(
  age = data$age,
  age_range = data$age_range
)

freq <- table(data_age$age_range)
freq

set.seed(123)
head(data_age)
st.sizes <- sample.size * freq / sum(freq)

st.1 <- strata(data_age, stratanames = c("age_range"),
               size = st.sizes, method = c("srswor"),
               description = TRUE)

st.1
help("strata")

st.sample1 <- getdata(data, st.1)

st.sample1


# cluster sampling
cl <- cluster(data, c("age"), size = 4, method = "srswor")
sample.6 <- getdata(data, cl)

table(sample.6$age)

mean_cluster <- mean(sample.6$age)



#sample size = 1000
# srswor
sample.size <- 1000
s <- srswor(sample.size,nrow(data))
sample.1 <- data[s != 0, ]

mean_srswor <- mean(sample.1$age)
#srswr
set.seed(153)
s <- srswr(sample.size, nrow(data))
sample.2 <- data[s != 0, ]
mean_srswr <- mean(sample.2$age)

#Systematic Sampling
N <- nrow(data)
n <- 1000

k <- ceiling(N / n)
k

r <- sample(k, 1)
r

s <- seq(r, by = k, length = n)
s

sample.3 <- data[s, ]
table(sample.3$age)
mean_systematic <- mean(sample.3$age)

#Systematic Sampling with unequal probabilities
pik <- inclusionprobabilities(data$age, sample.size)
s <- UPsystematic(pik)
sample.4 <- data[s != 0, ]
table(sample.4$age)

# Stratified sampling
freq <- table(data_age$age_range)
freq

set.seed(123)
head(data_age)
st.sizes <- sample.size * freq / sum(freq)

st.1 <- strata(data_age, stratanames = c("age_range"),
               size = st.sizes, method = c("srswor"),
               description = TRUE)

st.1

st.sample1 <- getdata(data, st.1)

st.sample1


# cluster sampling
cl <- cluster(data, c("age"), size = 4, method = "srswor")
sample.6 <- getdata(data, cl)

table(sample.6$age)

mean_cluster <- mean(sample.6$age)


# sample 
#Q6

# Word cloud for work type
# Install
install.packages("tm")  # for text mining
install.packages("SnowballC") # for text stemming
install.packages("wordcloud") # word-cloud generator 
install.packages("RColorBrewer") # color palettes
# Load
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
yes_stroke <- data[data$stroke == 1, ]

table(yes_stroke$work_type)

workCorpus <- Corpus(VectorSource(yes_stroke$work_type))
workCorpus <- tm_map(workCorpus, content_transformer(tolower))
wordcloud(workCorpus, max.words = 100, random.order = FALSE)

# Finding stroke presence from Patient ID
id <- readline(prompt="Enter the patient's id: ")
id <- as.integer(id)
id_stroke <- data[data$id == id, ]
if(id_stroke$stroke == 0){
  print(paste("The patient with ID", id, "does not have a stroke"))
} else{
  print(paste("The patient with ID", id, "has a stroke"))
}

