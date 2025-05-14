#write a program to compute the likelihood function of the binomial distribution assuming that the sample values are 1,1,1,4,1,2,4,2,2,0,7,1,1,0,2
x=c(1,1,1,4,1,2,4,2,2,0,7,1,1,0,2)
n=length(x)
f=function(x,theta)
{
  sum(x)/theta-(n^2-sum(x))/(1-theta)
  
}
fp=function(x,theta)
{
  -(sum(x)/theta^2)-(n^2-sum(x)/(1-theta^2))
  
}
th0=0.01;th0
th1=th0-f(x,th0)/fp(x,th0)
while (abs(th1-th0)>0.00001)
{
  th0=th1
  th1=th0-f(x,th0)/fp(x,th0)
  print(data.frame(th0,th1))
}
cat("\n the maximum likelihood estimate of binomial distribution is ",th0,"\n")



#exp 2
x=c(1,1,1,4,1,2,4,2,2,0,7,1,1,0,2);x
n=length(x)
f=function(x,theta)
{
  -n+(sum(x))/theta
  
}
fp=function(x,theta)
{
  -(sum(x)/theta^2)
  
}
th0=0.01;th0
th1=th0-f(x,th0)/fp(x,th0)
while (abs(th1-th0)>0.00001)
{
  th0=th1
  th1=th0-f(x,th0)/fp(x,th0)
  print(data.frame(th0,th1))
}
cat("\n the maximum likelihood estimate of binomial distribution is ",th0,"\n")



#exp3
x=c(1,1,1,4,1,2,4,2,2,0,7,1,1,0,2);x
n=length(x);n
s=1
f=function(x,mu)
{
  (sum(x-mu))/s
  
}
fp=function(x,mu)
{
  -n/s
  
}
mu0=0.01;mu0
mu1=mu0-f(x,mu0)/fp(x,mu0)
while (abs(mu1-mu0)>0.00001)
{
  mu0=mu1
  mu1=mu0-f(x,mu0)/fp(x,mu0)
  print(data.frame(mu0,mu1))
}
cat("\n the maximum likelihood estimate of binomial distribution is ",mu0,"\n")
mu0
mu1



#exp4
x=c(1,1,1,4,1,2,4,2,2,0,7,1,1,0,2);x
n=length(x)
mu=mean(x);mu
f=function(x,s)
{
  -n/(2*s)+(sum((x-mu)^2))/(2*s^2)
  
}
fp=function(x,s)
{
  n/(2*(s^2))-(sum((x-mu)^2))/(s^3)
  
}

s0=0.01;s0
s1=s0-f(x,s0)/fp(x,s0)
while (abs(s1-s0)>0.00001)
{
  s0=s1;
  s1=s0-f(x,s0)/fp(x,s0)
  print(data.frame(s0,s1))
}
cat("\n the maximum likelihood estimate of binomial distribution is ",s0,"\n")
s0
s1


#exp5
x=c(1,1,1,4,1,2,4,2,2,0,7,1,1,0,2)
n=length(x)
f=function(x,theta)
{
  (n/theta)-sum(x)
  
}
fp=function(x,theta)
{
  -n/(theta^2)
  
}
th0=0.01;th0
th1=th0-f(x,th0)/fp(x,th0)
while (abs(th1-th0)>0.00001)
{
  th0=th1
  th1=th0-f(x,th0)/fp(x,th0)
  print(data.frame(th0,th1))
}
cat("\n the maximum likelihood estimate of binomial distribution is ",th0,"\n")
th0
th1



#exp6

n=20
set.seed(8)
x=rcauchy(n,4,1)
cat("\n the generated sample from cachy distribution is,\n\n")
print(x)
f=function(x,theta)
{
  nr=2*(x-theta)
  dr=(1+(x-theta)*(x-theta))
  sum(nr/dr)
  
}
fp=function(x,theta)
{
  nr=(2*(((x-theta)^2)-1))
  dr=(1+(x-theta)*(x-theta))^2
  sum(nr/dr)
  
  
}
th0=1
th1=th0-(f(x,th0)/fp(x,th0))
while (abs(th1-th0)>0.01)
{
  th0=th1
  th1=th0-(f(x,th0)/fp(x,th0))
  
}
cat("\n the maximum likelihood estimator for the generated sample  is ",th0,"\n\n")




#exp7
set.seed(100)
x=runif(100,0,5)
cat("\n the generated sample from uniform distribution is,\n\n")
print(x)
mle=max(x)
cat("\n the maximum likelihood estimator for the generated sample  is ",mle,"\n\n")
theta=seq(0,10,0.01)
f=function(x,theta)
{ 
  if(theta>=max(x))
  {temp=1/(theta^length(x))
  }
  if(theta<max(x))
  {temp=0
  }
  temp
}
ptheta=vector()
for(i in 1:length(theta))
{
  ptheta[i]=f(x,theta[i])
}
plot(theta,ptheta,type="l")
abline(v=max(x))


#experiment 8- one sample t test
z = c(6.11,5.47,5.76,5.37,5.74,5.54,5.43,6.00,6.03,
      5.70,5.34,5.98,5.22,5.57,5.35,5.82,5.68,6.12,
      6.09,5.31)
ttest <- function(x,mu,alpha) {
  h0 = c("The true mean saponin is equal to 3400")
  h1 = c("The true mean saponin is less than 3400")
  n=length(x)
  tstat=(mean(x)-mu)/(sqrt(var(x)/n))
  talpha = qt(1-(alpha/2),n-1)
  pvalue = 2*(1-pt(abs(tstat),n-1))
  conf1 = (mean(x))-(talpha*sqrt(var(x)/n))
  confu = (mean(x))+(talpha*sqrt(var(x)/n))
  if(tstat>talpha){
    hypothesis = paste("Reject the null hypothesis and conclude that", h1)
  }
  else{
    hypothesis = paste("accept the null hypothesis and conclude that", h0)
  }
  output = list(tstat, talpha, pvalue, c(conf1, confu), hypothesis)
  names(output)=c("Tstatistics", "Table value", "P value", "95% confidence interval", "decision")
  output
}
alpha=0.01
ttest(z, 5.7, alpha)

# Compare with built-in t.test
t.test(z, mu=5.7, alternative="two.sided", conf.level=0.99)


#experiment9- two sample t test

lab1=c(0.304,0.305,0.302,0.310,0.294,0.293,0.300,0.296,0.300,0.298,0.316,0.304,0.303,0.309,0.305,0.298,0.292,0.294,0.301,0.307)
lab2=c(0.315,0.342,0.323,0.229,0.410,0.344,0.247,0.299,0.227,0.322,0.259,0.278,0.361,0.349,0.250,0.321,0.298,0.329,0.315,0.294)

m=length(lab1)
n=length(lab2)

ttest=function(x,y,alpha)
{
  h0=c("the two population means of area measurements are equal")
  h1=c("the two  population means of are measurements are nto equal")
  m=length(x)
  n=length(y)
  
  s=sqrt(((m-1)*var(x)+(n-1)*var(y))/(m+n-2))*(sqrt((1/m)+(1/n)))
  tstat=(mean(x)-mean(y))/(s)
  talpha=qt(1-(alpha/2),m+n-2)
  
  #p-value(optional)
  pvalue=pt(tstat,m+n-2)
  
  conf1=(mean(x)-mean(y))-(talpha*s)
  confu=(mean(x)-mean(y))+(talpha*s)
  
  if(tstat>talpha)
  {
    hypothesis=paste("we reject the null hypothesis and conclude that",h1)
  }
  else
  {
    hypothesis=paste("we accept the null hypothesis and conclude that",h0)
  }
  
  output= list(tstat,talpha,pvalue,c(conf1,confu),hypothesis)
  names(output)=c("Tstatistic","table value","pvalue","95% confidence level interval","decision")
  
  output
}

ttest(lab1,lab2,0.05)



#exp10

pre = c (14, 13, 4, 5,18, 3, 6, 11, 33, 11, 33, 5, 4, 15 , 4, 0, 7,2, 8, 4,4, 5)
post = c (0,26,2,4,8,1,0,3, 23,2,2,2, 6,0,3,2,3,0,0,13,24,6)
m= length (pre)
n = length ( post)
ttest = function (x, y, alpha)
{
  d=x-y
  n = length (d)
  s = sqrt (var (d) /n)
  tstat = (mean (d)) /(s)
  talpha = qt ( 1- (alpha/2), m-1)
  pvalue = 2*(1-pt (abs (tstat), m-1))
  conf1 = (mean (x) - mean (y)) - (talpha * s)
  confu = ( mean (x) - mean (y)) + (talpha * s)
   
  if (tstat > talpha)
  {
    hypothesis = "We reject the null Hypothesis - Hence, we can conclude that there is signifiant difference in
count of RBC before and after operation"
  }
  
  else
  {
    hypothesis =" We accept the null Hypothent that l there. is no signißrant difference betuxen the count of
RBC before and after operation"
  }
  
  output = list (tstat, talpha, pvalue, c (conf1, confu) , hypothesis)
  names (output) = c ( "tstatistic ","table value", "pvalue","95% confidence level", "decision")
  
  output
}
ttest (pre, post, 0.05)




#exp11

dal3 = read.csv(file.choose(),header=TRUE)
Chisquare = function(dal3)
{
  ob = as.matrix (dal3)
  r = nrow (ob)
  c = ncol (ob)
  df = (r-1) * (c- 1)
  
  
  ex = matrix (0,r,c)
  for (i in 1:r)
  {
    for (j in 1:c)
    {
      ex[i,j] = (sum(ob[i,]) * sum(ob[,j]))/(sum(ob))
    }
  }
  
  chisq = sum ((ob- ex)^2/ex)
  pvalue = 1 - pchisq (chisq, df)
  result = data.frame (chisq, df , pvalue)
  result
}
Chisquare (dal3)
chisq.test (dal3, correct = F)


#exp12


x=c(15.5,2.0,45.1,1.7,0.8,1.1,18.2,9.7,28.1,18.2,27.6,45.0,1.0,66.4,2.0,67.4,2.5,61.7,16.2,31.7,6.9,13.5,1.9,31.2,9.0,2.6,29.7,13.5,2.6,14.4,20.7,30.9,36.6,1.1,23.5,6.3,40.2,23.7,4.8,33.2,27.1,36.7,3.2,38.0,3.5,21.8,2.4)
n=length(x)
xmean=mean(x)
xmean
mu=30
ssd=sqrt(var(x))
ztest=function(n,mu,alpha)
{
  ztabulated=qnorm(1-alpha)
  z=(xmean-mu)/(ssd/sqrt(n))
  zcalculated=abs(z)
  
  if(zcalculated>ztabulated)
  {
    acceptance="Reject the null hypothesis and we conclude that population mean is equal to 30"
  }
  else
  {
    acceptance="accept the null hypothesis and we conclude that population mean is equal to 30"
  }
  output=list(zcalculated, ztabulated, acceptance)
  names(output)=c("calculated value","tabulated value","null hypothesis")
  
  output
  
}

ztest(n,mu,0.05)



#exp13

n = 49
mu = 30
sd = 11
xmean = 39

ztest = function(n, mu, alpha)
  {
  Ztabulated = qnorm(1 - alpha)
  Zcalculated = (xmean - mu) / (sd / sqrt(n))
  
  if (Zcalculated > Ztabulated)
    {
    acceptance = "Reject the null Hypothesis and we conclude that population mean is greater than 30"}
  else {
    acceptance = "Accept the null Hypothesis and we conclude that population mean is equal to 30"
  }
  
  output = list(Zcalculated, Ztabulated, acceptance)
  names(output) = c("Calculated value", "Tabulated value", "Null Hypothesis")
  return(output)
}
 ztest(n, mu, 0.05)

# RESULT:
# We reject the null hypothesis and we conclude that mean is greater than 30.

 
 
 #exp14

 
 # Data
 x <- c(19.8,10.1, 14.9, 7.5, 15.4, 15.4, 15.4, 18.5, 7.9, 12.7, 11.9, 11.4, 
        11.4, 14.1, 17.6, 16.7, 15.8, 19.5, 8.8, 13.6, 11.9, 11.4)
 
 # Box Plot
 boxplot(x, main = "Boxplot for the given random sample")
 
 # QQ Plot
 qqnorm(x, main = "QQ Plot for the given random sample")
 qqline(x, lwd=2)
 
 
 # Function to calculate confidence interval
 con <- function(x, alpha) 
   {
   n <- length(x)
   talpha <- qt(1 - (alpha / 2), n - 1)
   
   conf1 <- mean(x) - (talpha * (sqrt(var(x)/n)))
   confu <- mean(x) + (talpha * (sqrt(var(x)/n)))
   
   output <- list(talpha, c(conf1, confu))   # <-- close the list properly here
   names(output) <- c("Table value", "95% confidence interval")  # <-- outside
   output
 }
 
 con(x,0.05)
 

 
 
 
 #exp15
 
 dal2 = read.csv(file.choose(), header = T)
 dal2
 ot = lm(values ~ factor(trt), data = dal2)
 ot1 = anova(ot)
 ot1
 
 
 
 
 #exp16
 
 # Load the necessary library for reading CSV files
 dal4 = read.csv(file.choose(), header = TRUE)
 
 # Creating subsets based on categories
 a = which(dal4[,1] == "A")
 b = which(dal4[,1] == "B")
 c = which(dal4[,1] == "C")
 d = which(dal4[,1] == "D")
 
 # Test for checking normality
 shapiro.test(dal4[a, 2])
 shapiro.test(dal4[b, 2])
 shapiro.test(dal4[c, 2])
 shapiro.test(dal4[d, 2])
 
 # QQ plots for normality
 qqnorm(dal4[a, 2])
 qqline(dal4[a, 2])
 qqnorm(dal4[b, 2])
 qqline(dal4[b, 2])
 qqnorm(dal4[c, 2])
 qqline(dal4[c, 2])
 qqnorm(dal4[d, 2])
 qqline(dal4[d, 2])
 
 # Boxplots for visual comparison
 boxplot(dal4[a, 2])
 boxplot(dal4[b, 2])
 boxplot(dal4[c, 2])
 boxplot(dal4[d, 2])
 
 # Kruskal-Wallis test
 kruskal.test(value ~ group, data = dal4)
 
 
 #exp17
 # Load data from a CSV file
 data = read.csv(file.choose(), header = TRUE)
 
 # Data extraction
 a = data[, 1]
 b = data[, 2]
 
 # Create a new vector
 c = as.vector(a-b)
 
 # Normality tests
 qqnorm(c)
 qqline(c)
 
 # Shapiro-Wilk normality test
 shapiro.test(c)
 
 # Boxplot for visualization
 boxplot(c)
 
 # Wilcoxon test
 wilcox.test(a, b, mu = 0, paired = TRUE, alternative = "less")
 
 
 #exp18
 n <- 100      # Sample size
 P <- 25 / 100 # Proportion of successes
 p <- 35 / n   # Sample proportion
 Q <- 1 - p 
 # Function to calculate Z-statistics
 ztest <- function(n, P, p, Q, alpha) {
   Ztabulated <- qnorm(1 - alpha) # Z-tabulated value
   Zcalculated <- (p - P) / sqrt((P * Q) / n) # Z-calculated value
   
   # Checking conditions for acceptance or rejection of the null hypothesis
   if ((n * P > 5) & (n * (1 - P) > 5)) {
     if (Ztabulated < Zcalculated) {
       acceptance <- "Reject the Null Hypothesis and we conclude that there is a deterioration of quality at 5% level of significance"
     } else {
       acceptance <- "Accept the Null Hypothesis and we conclude that there is no deterioration of quality at 5% level of significance"
     }
     
     # Creating output
     output <- list(Zcalculated = Zcalculated, Ztabulated = Ztabulated, Decision = acceptance)
     names(output) <- c("Calculated value", "Tabulated value", "Decision")
   } else {
     output <- "It will not follow normal distribution"
   }
   return(output)
 }
  ztest(n, P, p, Q, 0.05)
 
#exp19
  
  n1 <- 100  # Sample size for group 1
  n2 <- 80   # Sample size for group 2
  x1 <- 60   # Number of successes in group 1
  x2 <- 40   # Number of successes in group 2
  
  # Calculate proportions
  p1 <- x1 / n1
  p2 <- x2 / n2
  P <- (x1 + x2) / (n1 + n2)
  Q <- 1 - P
  
  # Z-test function
  z_test <- function(n1, n2, p1, p2, P, Q, alpha) {
    Ztabulated <- qnorm(1 - (alpha / 2))
    Zcalculated <- (p1 - p2) / sqrt((P * Q) * ((1/n1) + (1/n2)))
    
    if ((n1 * p1 > 5) && (n1 * (1 - p1) > 5) && (n2 * p2 > 5) && (n2 * (1 - p2) > 5)) {
      if (abs(Zcalculated) > Ztabulated) {
        acceptance <- "Reject the Null Hypothesis and conclude that there is a significant difference"
      } else {
        acceptance <- "Accept the Null Hypothesis as there is no significant difference"
      }
      # Output the results
      output = list(Zcalculated, Ztabulated, acceptance)
      names(output) = c("Calculated value", "Tabulated value", "Null Hypothesis")
    } else {
      output = "Sample size is too small; normal approximation is not valid"
    }
    
    return(output)
  }
  
  # Execute the test
  z_test(n1, n2, p1, p2, P, Q, 0.05)
  
  
  
  #exp20
  
  s = 0.0153
  sigma = 0.01
  
  var = function(n, s, sigma, alpha) {
    tabulated = qchisq(1 - alpha, n - 1)
    calculated = (n - 1) * s^2 / sigma
    
    if (tabulated < calculated) {
      acceptance = "Reject the Null Hypothesis and we can conclude that the variance of fill volume exceed 0.01"
    } else {
      acceptance = "Accept the Null Hypothesis and we can conclude that the variance of fill volume does not exceed 0.01"
    }
    
    output = list(calculated, tabulated, acceptance)
    names(output) = c("Calculated Value", "Tabulated Value", "Null Hypothesis")
  
  output
  }
var(n,s,sigma,0.05)