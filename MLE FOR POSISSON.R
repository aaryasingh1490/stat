#write a progrm to compute the likelihood function of poisson distribution assuming that the sample values are 1,1,1,4,1,2,4,2,2,0,7,1,1,0,2.
x=c(1,1,1,4,1,2,4,2,2,0,7,1,1,0,2)
n=length(x)
f=function(x,theta)
{
 -n+(sum(x))/theta
}

fp=function(x,theta)
{
  (-sum(x)/theta^2)
}

th0=0.01;th0
th1=th0-f(x,th0)/fp(x,th0)

while(abs(th1-th0)>0.00001)
{
  th0=th1
  th1=th0-f(x,th0)/fp(x,th0)
  print(data.frame(th0,th1))
}
cat("\n the maximum likelihood estimator of poisson distribution is:",th0,"\n")
