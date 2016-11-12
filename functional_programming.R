functional_programming.R
##Anonymous functions
df<- data.frame(replicate(6, sample(c(1:10, -99), 6, rep = TRUE)))
names(df)<- letters[1:6]
fix_missing<- function(x){
  x[x == -99]<- NA
  x
}
df[]<- lapply(df, fix_missing)
fix_missing_99<- function(x){
  x[x == -99]<- NA
  x
}
##closure
missing_fixer<- function(na_value){
  function(x){
    x[x == na_value]<- NA
    x
  }
}
fix_missing_99<- missing_fixer(-99)
summary<- function(x){
  funs<- c(mean, median, sd, mad, IQR)
  lapply(funs, function(f) f(x, na.rm = TRUE))
}
lapply(df, summary)
##list of functions
compute_mean<- list(
  base = function(x) mean(x),
  sum = function(x) sum(x)
)
x<- runif(1e5)
lapply(compute_mean, function(f) f(x))
call_fun<- function(f, ...) f(...)
lapply(compute_mean, call_fun, x)
##functionals
randomise<- function(f) f(runif(1e3))
randomise(median)
lapply2<- funciton(x, f, ...){
  out<- vector("list", length(x))
  for(i in seq_along(x)){
    out[[i]]<- f(x[[i]], ...)
  }
  out
}