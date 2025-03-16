#write a progrm to compute the likelihood function of cauchy distribution assuming that the sample values are 1,1,1,4,1,2,4,2,2,0,7,1,1,0,2.
n=20
set.seed(8)
x=rcauchy(n,4,1)
cat("\n the generated sample from the cauchy distribution is \n\n")
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


th0=1;th0
th1=th0-f(x,th0)/fp(x,th0)

while(abs(th1-th0)>0.00001)
{
  th0=th1
  th1=th0-f(x,th0)/fp(x,th0)
  print(data.frame(th0,th1))
}

cat("\n the maximum likelihood estimator for the generated sample is",th0,"\n\n")
