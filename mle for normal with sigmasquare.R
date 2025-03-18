x=c(1,1,1,4,1,2,4,2,2,0,7,1,1,0,2)
x
n=length(x)
mu=mean(x)
mu
f=function(x,s)
{
  -n/(2*s)+(sum((x-mu)^2))/(2*s^2)
}

fp=function(x,s)
{
  n/(2*(s^2))-(sum((x-mu)^2))/(s^3)
}

th0=0.01
th0
th1=th0-f(x,th0)/fp(x,th0)

while(abs(th1-th0)>0.00001)
{
  th0=th1
  th1=th0-f(x,th0)/fp(x,th0)
  print(data.frame(th0,th1))
}
cat("\n the maximum likelihood estimator of normal distribution is:",th0,"\n")
