#1.1) Pseudo Code
obj.fun = function(x) {exp(-x) + x^2}
dev.fun = function(x) {-exp(-x) + 2*x}


a = -10;b = 10;iter.max = 100;eps = 1e-9

for(iter in 1:iter.max){
  if( dev.fun((a+b)/2) * dev.fun(a) > 0 ){
    a = (a+b)/2
  }else{ b = (a+b)/2 }
  if( abs(b-a) < eps) break
}

