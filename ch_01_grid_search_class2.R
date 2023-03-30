#1)우리의 궁극적인 목표 : mean((y.vec - m)^2)
#1.1)
z.vec = runif(100)*100
y.vec = runif(100)*100

risk.fun = function(y.vec, m){ mean((y.vec - m)^2) }
risk.fun(y.vec = z.vec, 3)

r.vec = NULL #초기값 설정 해주는 거

## graph 그리기
for (m in m.vec){
  r.vec = c(r.vec, risk.fun(z.vec,m)) #python : concat
}

plot(m.vec, r.vec)

#1.2) 
#모수 m 에서 최소값을 가지는건 우리가 알고 있다. 하지만 모수를 추정하지 못하므로 최소값 : 최대값으로 범위를 정해주면 되는거 아니냐?
#또한 함수화까지 진행해보자.
z.vec = runif(100)*100
plot.square.risk = function(y.vec, m){
  risk.fun = function(y.vec, m){ mean((y.vec - m)^2) }
  m.vec = seq(min(y.vec), max(y.vec), length.out = 100) ##여기가 최소~최대로 바뀜
  r.vec = NULL #초기값 설정 해주는 거

  ## graph 그리기
  for (m in m.vec){
    r.vec = c(r.vec, risk.fun(y.vec,m)) #python : concat
  }
  
  plot(m.vec, r.vec)
}

#1.3)
rm(list-lst())

risk.fun = function(y.vec, m){ mean((y.vec - m)^2) }
m.vec = seq(min(y.vec), max(y.vec), length.out = 100)
r.vec = NULL 

for (m in m.vec){
  r.vec = c(r.vec, risk.fun(z.vec,m))
}

plot(m.vec, r.vec)

## plot을 통해 구간을 설정할 수 있다 이제는
a = min(y.vec)
b = max(y.vec)
eps = 1e-7
iter.max = 10

for (iter in 1:iter.max){
  m.vec = seq(a, b, length.out = 10) #함수값을 계산할 값
  r.vec = m.vec * 0 ### 함수값을 저장할 변수
  for (i in 1:10){
    r.vec[i] = risk.fun(y.vec, m.vec[i]) #m값을 함수(MSE 구하는 함수)에 넣는 과정
  }
  opt = which.min(r.vec) #10개 함수값 중에서 어느 함수값이 최소야?
  a = m.vec[opt-1]
  b = m.vec[opt+1]
  
  if (abs(b-a) < eps) break #구간 종료 조건
}

#1.4) 이제는 데이터만 집어넣어도 구간의 평균을 산출할 수 있는 함수를 만들자

rm(list-lst())

my.mean = function(sam, eps = 1e-7, iter.max = 10, len = 10){
  #risk.fun = function(y.vec, m){ mean((y.vec - m)^2) } #sam으로 고칠 필요 X
  risk.fun = function(y.vec, m){ mean(abs(y.vec - m)) } #median ver도 가능해
  
  a = min(sam)
  b = max(sam)
  
  for (iter in 1:iter.max){
    m.vec = seq(a, b, length.out = len) #함수값을 계산할 값
    r.vec = m.vec * 0 ### 함수값을 저장할 변수
    for (i in 1:len){
      r.vec[i] = risk.fun(sam, m.vec[i]) #m값을 함수(MSE 구하는 함수)에 넣는 과정
    }
    opt = which.min(r.vec); a = m.vec[opt-1] ; b = m.vec[opt+1]
    
    if (abs(b-a) < eps) break #구간 종료 조건
  }
  return ((a+b)/2) 
}
### 결과검증
x = rnorm(10)
my.mean(x, 1e-10, 100, 5) # iter = 10 일 때는 두 값이 같지 않다는 점!
mean(x)

#1.5) 그러면 Median / Mean 을 hyperparameter로 진행해보자

rm(list-lst())

my.mean = function(sam, mod = 'mean', eps = 1e-7, iter.max = 10, len = 10){
  if(mod == 'mean'){
    risk.fun = function(y.vec, m){ mean((y.vec - m)^2) } 
  }
  if(mod == 'median'){
    risk.fun = function(y.vec, m){ mean(abs(y.vec - m)) } 
  }
  
  a = min(sam)
  b = max(sam)
  
  for (iter in 1:iter.max){
    m.vec = seq(a, b, length.out = len) #함수값을 계산할 값
    r.vec = m.vec * 0 ### 함수값을 저장할 변수
    for (i in 1:len){
      r.vec[i] = risk.fun(sam, m.vec[i]) #m값을 함수(MSE 구하는 함수)에 넣는 과정
    }
    opt = which.min(r.vec); a = m.vec[opt-1] ; b = m.vec[opt+1]
    
    if (abs(b-a) < eps) break #구간 종료 조건
  }
  return ((a+b)/2) 
}
### 결과검증
x = rnorm(10)
my.mean(x, 'mean',1e-10, 10, 5) # iter = 10 일 때는 두 값이 같지 않다는 점!
mean(x)

#### 숙제는 Huber, Lasso 도 mode로 설정하는 것이 과제. Huber랑 Lasso는 delta, lambda 가 hyperparameter가 있어야 하기 때문에 delta, lambda도 마찬가지로 hyperparameter화 해야한다.  #####
