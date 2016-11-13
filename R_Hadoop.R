library(rhdfs)
library(rmr2)

data_generator<- function(m){
  n<- 5 + rpois(1,5)
  as.data.frame(cbind(sample(1:3, n, replace = T), matrix(rnorm(n*m, 0, 1), n, m)))
}

A<- data_generator(3)
colnames(A)<- c("key", "A1", "A2", "A3")

B<- data_generator(4)
colnames(B)<- c("key", "B1", "B2", "B3")

C<- data_generator(3)
colnames(C)<- c("key", "C1", "C2", "C3")

A_wk<- split(A,A$key)
B_wk<- split(B,B$key)
C_wk<- split(C,C$key)

Total<- keyval(c(names(A_wk),names(B_wk),names(C_wk)),c(A_wk,B_wk,C_wk))

Demo<- mapreduce(
    input = to.dfs(Total),
    reduce = function(k,v){
      test<- split(v,sapply(v,function(x) paste(colnames(x),collapse = "")))
      test<- lapply(test,function(x) Reduce(rbind,x))
      keyval(k,list(test[[1]],test[[2]],test[[3]]))
    }
)

from.dfs(Demo)