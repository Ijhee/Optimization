# F(x), f(x) 값을 구할 수 있는걸 확인
dnorm(3,3,1) #f(x)
pnorm(3,3,1) #F(x)

#정규분포에서 a,b 를 찾는데 특화된 function이라면
## My Code
##사용할 때 조심할 점 : 예를 들어 Lambda가 dnorm보다 커짐으로서 값이 구해지지 않을 경우가 생긴다.
arg.fun = function(lam, mu, sig, iter.max, eps){
  #if (lam > dnorm(mu,mu,sig)){print('lamda must be smaller than dnorm(mu,mu,sig)') break} 
  a = -10^5 ; b = mu
  for (i in 1: iter.max){if ((dnorm((a+b)/2, mu, sig)-lam) * (dnorm(a, mu, sig)-lam) > 0){
    a = (a+b)/2
  }else{ b = (a+b)/2}
   if (abs(b-a) < eps) break 
  }
  return((a+b)/2)
}
arg.fun(0.1,0,1,100,1e-7)

#대칭이라면 그냥 Mu를 기준으로 a값을 더해주기만 하면 된다.
#아래 함수는 lambda 값을 bisection을 통해 구하는 과정
int.fun = function(alp, mu, sig, iter.max, eps){
  a = 10^-5 ;  b = dnorm(mu,mu,sig) - 10^5 #정규분포일 경우 최대값에서 조금만 아래로 내려와서 a,b의 값이 두개 생길 수 있도록 설정한다.
  for (iter in 1:iter.max){
    #일단 a, b 값을 구해서 lambda값을 구할 수 있도록 설정
    a.lam = arg.fun(a,mu,sig,eps,iter.max)
    c.lam = arg.fun((a+b)/2,mu,sig,eps,iter.max) #중앙값
    #이제 g 함수에 a, c 대입할거야
    #-a.lam = b.lam으로 생각하는듯 -> b.lam = a.lam + mu 아님?
    if((pnorm(-a.lam,mu,sig) - pnorm(a.lam,mu,sig) - alp) *
       (pnorm(-c.lam,mu,sig) - pnorm(c.lam,mu,sig) - alp) > 0){
         a = (a+b)/2}
    else{b = (a+b)/2}
    if (abs(b-a) < eps) break
  }
  return (c(a.lam, (a.lam+mu)))
  }

int.fun(0.1,0.1,0,1,100,1e-7)

#내생각 반영
int.fun = function(alp, mu, sig, iter.max, eps){
  a = 10^-5 ;  b = dnorm(mu,mu,sig) - 10^5 #정규분포일 경우 최대값에서 조금만 아래로 내려와서 a,b의 값이 두개 생길 수 있도록 설정한다.
  for (iter in 1:iter.max){
    #일단 a, b 값을 구해서 lambda값을 구할 수 있도록 설정
    a.lam = arg.fun(a,mu,sig,eps,iter.max)
    c.lam = arg.fun((a+b)/2,mu,sig,eps,iter.max) #중앙값
    #이제 g 함수에 a, c 대입할거야
    #-a.lam = b.lam으로 생각하는듯 -> b.lam = a.lam + mu 아님?
    if((pnorm((a.lam + mu),mu,sig) - pnorm(a.lam,mu,sig) - alp) *
       (pnorm(-c.lam,mu,sig) - pnorm(c.lam,mu,sig) - alp) > 0){
      a = (a+b)/2}
    else{b = (a+b)/2}
    if (abs(b-a) < eps) break
  }
  return (c(a.lam, (a.lam+mu)))
}