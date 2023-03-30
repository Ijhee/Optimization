## MLE
log.fun = function(y.vec, p){
  res = -sum(y.vec) * log(p/(1-p))  - length(y.vec)*log(1-p)
  return (res)
}

##그림을 한번 그려보자
y.vec = c(1,1,1,0,0,0)
log.fun(y.vec, 0.5) #지금까지는 p = 0.5일 경우에만 진행한것

## 그러면 p = 0~1까지의 경향이 어떻게 되는지 확인해봐자
p.vec = seq(0,1,0.01)
plot(log.fun(y.vec,p.vec))

## y.vec을 바꿔서 그려보기
y.vec = c(rep(1,90), rep(0,10))
plot(log.fun(y.vec,p.vec))

## 그래프 형태를 확인했으니 golden section
a = 0 ; b = 1; iter.max = 1000; eps = 1e-15; alpha = (-1+sqrt(5))/2
for (i in 1:iter.max){
  c = alpha*a + (1-alpha)*b
  d = (1-alpha)*a + alpha*b
  
  if (log.fun(y.vec, c) > log.fun(y.vec,d)){
    a = c ; b = b
  }
  else{
    b = d; a = a
  }
  if (abs(b-a) < eps) break
}
(a+b)/2
i
### optimal golden section
a = 0 ; b = 1; iter.max = 1000; eps = 1e-15; alpha = (-1+sqrt(5))/2
c = alpha*a + (1-alpha)*b; c.val = log.fun(y.vec,c)
d = (1-alpha)*a + alpha*b; d.val = log.fun(y.vec,d)

for (i in 1:iter.max){
  
  if (c.val > d.val){
    a = c ; c = d; c.val = d.val
    d = (1-alpha)*a + alpha*b; d.val = log.fun(y.vec,d)
  }
  else{
    b = d; d = c; c.val = d.val;
    c = alpha*a + (1-alpha)*b; c.val = log.fun(y.vec,c)
  }
  if (abs(b-a) < eps) break
}
(a+b)/2
i

##교재
rm(list=ls())
max.like.fun = function(y.vec, fam = 'normal', mod = 'fast',
                        iter.max=100,eps=1e-10,alpha= (-1+sqrt(5))/2){
  if(fam=='normal'){ a= min(y.vec); b= max(y.vec)
    loss.fun = function(y.vec,mu){return(mean((y.vec-mu)^2))}
  }
  if(fam=='bernoulli'){a=1e-3;b=1-a
    loss.fun = function(y.vec,p){
      return(-mean(y.vec*log(p) + (1-y.vec)*log(1-p)))}
  }
  c = alpha*a+(1-alpha)*b; c.val = loss.fun(y.vec,c)
  d = alpha*b+(1-alpha)*a; d.val = loss.fun(y.vec,d)
  for(iter in 1:iter.max){
    if(abs(b-a)<eps) break
    if(mod == 'fast'){if(c.val>d.val){
    a = c; c = d; c.val = d.val
    d = alpha*b+(1-alpha)*a; d.val = loss.fun(y.vec,d)
  }else{
    b = d; d = c; d.val = c.val
    c = alpha*a+(1-alpha)*b; c.val = loss.fun(y.vec,c)}
  }
  else{
    c = alpha*a+(1-alpha)*d; c.val = loss.fun(y.vec,c)
    d = alpha*b+(1-alpha)*a; d.val = loss.fun(y.vec,d)
    if(c.val>d.val)a = c else b =d
  }}
  return(list(fam=fam, mle =a))
}
y.vec = c(rep(1,90), rep(0,10))
max.like.fun(y.vec, fam = 'bernoulli', mod = 'fast',
                        iter.max=100,eps=1e-10,alpha= (-1+sqrt(5))/2)
