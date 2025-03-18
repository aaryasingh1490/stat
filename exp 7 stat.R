set.seed(1000)
x=runif(100,0,5)
cat("the generated sample is:")
print(x)
mle=max(x)
theta=seq(0,10,0.01)
f=function(x,theta)
{
  if(theta>=max(x))
  {
    temp=1/(theta^length(x))
  }
  if(theta<max(x))
  {
    temp=0
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