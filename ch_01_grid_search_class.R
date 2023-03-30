#### goal : f(x) = exp(-x) + x^2 의 최솟값 찾기

### grid search 하기 위해서는 search 할 범위를 정해야 함 -> 그림을 통해 범위를 정해보자
#함수 정의
obj.fun = function(x){
  exp(-x) + x^2 #return 없을 때는 마지막에 나온 값을 return
}

x.vec = seq(-5,10, length.out = 100)
y.vec = x.vec * Inf # x.vec 과 길이가 맞는 임의의 벡터를 생성한다. 

obj.fun(x.vec[1])
obj.fun(x.vec[100])
obj.fun(x.vec[50])
obj.fun(x.vec[10])
### 이러면 1 / 100 / 50 내에 샌드위치 정리가 성립된다 -> 현재는 -10~10 사이에 최솟값이 존재함을 알 수 있다.
### 따라서 a, b 값을 -10 , 10 으로 구한 상태! - seq(-10, 10)이여서
### x.vec 100개의 원소값에 대한 함수값을 구하고자 한다.
pos = 0 #y의 인덱스 설정
for(x in x.vec){
  pos = pos + 1
  print(pos) #pos 잘 먹히는지 확인
  y.vec[pos] = obj.fun(x)
  print(y.vec[pos])
}

plot(x.vec, y.vec) # seq(-10 ~ -5) 에서의 값 scale이 너무 커서, 그래프가 정확하게 나오지 않음 & -5 ~ 10으로 그리면 잘 나옴

### 쉽게 쓰는 코드
obj.fun = function(x){
  exp(-x) + x^2 #return 없을 때는 마지막에 나온 값을 return
}
x.vec = seq(-5,10, length.out = 100)
y.vec = obj.fun(x.vec) #그냥 순서대로 들어감

plot(x.vec, y.vec)

### 이제 구간은 정했다. BUT 너무 오래 걸림
a = -5 ; b = 10; eps = 10^(-7); m = round(1/eps)
x.vec = seq(a, b, length.out = m)
y.vec = rep(NA, m)
for (i in 1:m){
  y.vec[i] = obj.fun(x.vec[i])
}

## For문을 사용하지 말자 -> 그래도 점이 너무 많아서 plot 그리기는 힘들다
a = -5 ; b = 10; eps = 10^(-7); m = round(1/eps)
x.vec = seq(a, b, length.out = m)
y.vec = obj.fun(x.vec)
#plot(x.vec, y.vec)

## For문을 사용하지 말자 -> 그래도 점이 너무 많아서 plot 그리기는 힘들다
a = -5 ; b = 10; eps = 10^(-3); m = round(1/eps)
x.vec = seq(a, b, length.out = m)
y.vec = obj.fun(x.vec)
plot(x.vec, y.vec) #range 좀 줄임

### 최소값을 구해보자
opt = which.min(y.vec) #최소값의 index
x.vec[opt]
abline(v = x.vec[opt]) #최소값에 줄을 그려주기

####다시 eps = 10^(-7)로 진행해보자

iter.max = 100 #반복 횟수 지정
a = -5; b = 10; m = 10  #inital은 for 문 밖으로 빼주자

for(iter in 1:iter.max){
  print(iter)
  x.vec = seq(a, b, length.out = m)
  print(1)
  y.vec = obj.fun(x.vec); opt = which.min(y.vec) #opt = min's index of y.vec
  print(2)
  print(opt)
  a = x.vec[opt-1] #change range
  print(a)
  b = x.vec[opt+1]
  print(b)
  print(c(a,b))
}

#### 최소값이 y.vec의 맨 앞에 있어 opt = 1 이거나, 마지막에 있을 경우  error가 발생하므로 조건문으로 규제한다.
###근데 구간 내에서 잘 굴러가다가 왜 갑자가ㅣ opt = 1로 돼서 error가 뜨는지 모르겠네

iter.max = 100 #반복 횟수 지정
a = -5; b = 10;

#inital은 for 문 밖으로 빼주자

for(iter in 1:iter.max){
  x.vec = seq(a, b, length.out = m)
  y.vec = obj.fun(x.vec); opt = which.min(y.vec) 
  
  if(opt == 1 | opt == m) break
  a = x.vec[opt-1]
  b = x.vec[opt+1]
}
x.opt = (a+b) / 2 #왜 평균값이지? 못들음

### 범위가 지금 같이 잡혀있을 경우 1000번까지 다 돌아간다. 
### 조건문을 설정해줄 경우 지금과 같이 iter 8번만 돌면 끝난다.

iter.max = 100 #반복 횟수 지정
a = -10; b = 5; m = 20; eps = 1e-7 #inital은 for 문 밖으로 빼주자
 
for(iter in 1:iter.max){
  x.vec = seq(a, b, length.out = m)
  y.vec = obj.fun(x.vec); opt = which.min(y.vec) 
  
  if(opt == 1 | opt == m) break
  a = x.vec[opt-1]
  b = x.vec[opt+1]
  
  if (abs(b-a)<eps) break
  print(c(a,b))
  print(iter)
}
x.opt = (a+b) / 2 #왜 평균값이지? 못들음


