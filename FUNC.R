# 함수목록-----------------------------------------------------------

oddcount <- function(x){
  count <- 0
  for (i in 1:length(x)){
    if(x[i] %% 2 == 1) count <- count+1# a%%k는 a를 k로 나눈 나머지
  }
  return(count)
}

evencount<- function(x){
  count <- 0
  for (i in 1:length(x)){
    if(x[i] %% 2 == 0) count <- count+1# a%%k는 a를 k로 나눈 나머지
  }
  return(count)
}

reg <- function(y, x1, x2){
  X <- cbind(rep(1,length(y)), x1, x2)
  b <- solve(t(X) %*% X) %*% (t(X) %*% y)
  r <- y - X %*% b
  y.1 <- y - mean(y)
  R.squared <- 1 - sum(r*r)/sum(y.1*y.1)
  return(list(beta=round(b, 5), R.squared=round(R.squared, 4)))
}

blurpart <- function(img,rows,cols,q){
  lrows <- length(rows)
  lcols <- length(cols)
  newimg <- img
  randomnoise <- matrix(nrow=lrows, ncol=lcols, runif(lrows*lcols))
  newimg@grey[rows,cols] <- (1-q) * img@grey[rows,cols]+ q*
    randomnoise
  return(newimg)
}

summarize <- function(x){
  mean.sd <- c(mean=mean(x),sd=sd(x))
  fivenum <- quantile(x,prob=c(0,0.25,0.5,0.75,1))
  lower <- fivenum[2]-1.5*(fivenum[4]-fivenum[2])
  upper <- fivenum[4]+1.5*(fivenum[4]-fivenum[2])
  outliers <- c(x[x<lower],x[x>upper])
  list(mean.sd=mean.sd,fivenum=fivenum,outliers=outliers)
}

corplot <- function(x,y,plotit){
  if (plotit == TRUE) plot(x,y)
  cor(x,y)
}

Eratosthenes <- function(n){
  # n까지의 모든 소수를 출력 (에라토스테네스의 체 알고리즘 이용)
  if (n >=2 ){
    sieve <- seq(2,n)
    primes <- c()
    for (i in seq(2,n)) {
      if (any(sieve==i)) {
        primes <- c(primes, i)
        sieve <- c(sieve[(sieve %% i) !=0], i)
      }
    }
    return(primes)
  } else{
    stop("Input values of n should be at least 2.")
  }
}

annuityAmt <- function(n,r,i) {
  r*((1+i)^n - 1)/i
}

baseball <- function(prob){
  outs <- 0
  hits <- 0
  repeat{
    x <- sample(c(0,1), 1, prob=prob)
    if (x == 1) hits <- hits + 1 else outs <- outs + 1
    if (outs >= 3) break
  }
  return(hits) 
}
baseball.1 <- function(prob){
  outs <<- 0
  hits <- 0
  repeat{
    x <- sample(c(0,1), 1, prob=prob)
    if (x == 1) hits <- hits + 1 else outs <<- outs + 1
    if (outs >= 3) break
  }
  return(hits)
}
baseball.2 <- function(prob){
  outs <- 0
  hits <- 0
  repeat{
    x <- sample(c(0,1), 1, prob=prob)
    if (x == 1) hits <- hits + 1 else outs <- outs + 1
    if (outs >= 3) break
  }
  return(list(hits=hits, outs=outs))
}

blackjackscore <- function(x) {
  if (x %in% c("J","Q","K")) return(10)
  else if (x == "A") return(11)
  else return(as.numeric(x))
}
blackjackscore.1 <- function(x) {
  if (x %in% c("J","Q","K")) return(10)
  else if (x == "A") return(1)
  else return(as.numeric(x))
}

# 스니펫 ---------------------------------------------------------------------
# code_clear
rm(list=ls())

# vscode
install.packages('remote')
remotes::install_github("anthonynorth/rscodeio")
rscodeio::install_theme()


