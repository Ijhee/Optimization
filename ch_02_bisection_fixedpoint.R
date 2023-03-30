g.fun = function(x,a){ (a/x + x) / 2}

sq.fun = function(a,iter.max,eps, initial){
  x = initial
  for (iter in 1: iter.max){
  ox = x #이전 x
  x = g.fun(x,a) #함수값에 집어넣은 현재 x
  if (abs(ox-x) < eps) break
  }
  return(x)
}
sq.fun(2,100,1e-10,1)
 