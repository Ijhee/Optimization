c.fun = function(a.mat, lam){
  b.mat = a.mat - lam * diag(ncol(a.mat)) #diag : Identify Matrix 만드는 함수이며, square matrix인 a.mat과 동일한 길이의 matrix를 만들고자 한다.
  return(det(b.mat)) #최종 : det(A-lam*I)
}

x.mat = matrix(rnorm(10), 5, 2) #(5,2) matrix
a.mat = t(x.mat)%*%x.mat #A = X^TX 이며, t->transpose
#이젠 우리는 det(A-lam*I) = 0 이 되는 지점을 찾는 것이 목표.
my.eval.fun = function(){
  
}

## 임의의 벡터를 생성해서 변화하는 lambda에 따른 함수값 추정
l.vec = seq(-10,10,length.out = 100)
c.vec = NULL

for (lam in l.vec){
  c.vec = c(c.vec,c.fun(a.mat, lam))
}
#det(A-lam*I)는 (-1)^n*lam^n인데, n=2이므로 2차식이 형성된다.
plot(l.vec, c.vec)

######## 교수님 풀이
x.mat = matrix(rnorm(100), 20 ,5) #원래 5,2 차원
a.mat = t(x.mat)%*%x.mat
c.fun = function(a.mat, lam){
  return(det(a.mat-lam*diag(ncol(a.mat))))
}
c.fun(a.mat, 1) #확인
k =1000
l.vec = seq(0,30,length.out = k) #원래 -10,10
c.vec = l.vec * Inf
for (id in 1:k){
  c.vec[id] = c.fun(a.mat, l.vec[id])
}
plot(l.vec, c.vec); abline(h=0)

##0을 지나가는 지점이 2개 생길거고, [(a,b),(a,b)]로 저장하고 싶다(2*2) 행렬로
it.mat = matrix(0,2,2)
for (id in 1:99){
  if (c.vec[id] * c.vec[id+1] < 0){
    it.mat[id,] = c(l.vec[id], l.vec[id+1])
  }
}

## id로 인해 오류 발생 -> 해결
it.mat = NULL
for (id in 1:k-1){
  if (c.vec[id] * c.vec[id+1] < 0){
    it.mat = rbind(it.mat,c(l.vec[id], l.vec[id+1]))
  }
}


