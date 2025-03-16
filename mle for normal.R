#write a progrm to compute the likelihood function of normal distribution assuming that the sample values are 1,1,1,4,1,2,4,2,2,0,7,1,1,0,2.
x=c(1,1,1,4,1,2,4,2,2,0,7,1,1,0,2)
n=length(x)
s=1
f=function(x,mu)
{
  sum(x-mu)/s
}

fp=function(x,mu)
{
  -n/s
}

th0=0.01;th0
th1=th0-f(x,th0)/fp(x,th0)

while(abs(th1-th0)>0.00001)
{
  th0=th1
  th1=th0-f(x,th0)/fp(x,th0)
  print(data.frame(th0,th1))
}
cat("\n the maximum likelihood estimator of normal distribution is:",th0,"\n")
