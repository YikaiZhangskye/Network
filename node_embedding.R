n = 1000
k = 2
pi = c(0.5,0.5)
c = 5
lambda = 0.8
num_paths =1
length_path = 10
emb_dim = 8
winsize = 5
p = c/n
q = c*(1-lambda)/n
D_p = NULL
D_m = NULL
m = 3

w = matrix(c(p,q,q,p),2,2)
y = c(rep(0,n/2),rep(1,n/2))
A = diag(0,n)
for(i in 1:(n-1)){
  for(j in (i+1):n){
    if(y[i]==y[j]){
      if(runif(1)<p){A[i,j]=A[j,i]=1}
      else{A[i,j]=A[j,i]=0}
    }
    else{
      if(runif(1)<q){A[i,j]=A[j,i]=1}
      else{A[i,j]=A[j,i]=0}
    }
  }
}


degree = colSums(A,1)

#Random walk
for(i in which(degree!=0)){
  start = i
  rw = NULL
  rw = c(rw,start)
  for(j in 1:(length_path-1)){
    if(length(which(A[start,]==1))>1){
      end = sample(which(A[start,]==1),1)
    }
    else{
      end = which(A[start,]==1)
    }
    rw = c(rw,end)
    start = end
  }
  for(j in 1:length_path){
    for(ii in max(1,j-winsize):min(length_path,j+winsize)){
      D_p = c(D_p,rw[ii],rw[j])
      for (jj in 1:m){
        D_m = c(D_m,rw[ii],sample(rw,1))
      }
    }
  }
}

#D+:
D_p = matrix(D_p,ncol=2,byrow = TRUE)
#D-:
D_m = matrix(D_m,ncol=2,byrow = TRUE)

#Optimize
mu = matrix(rnorm(n = n*emb_dim,mean = 0,sd = 1),nrow = n,ncol = emb_dim)
for (i in 1:300){
  xp = sample(1:dim(D_p)[1],0.2*dim(D_p)[1])
  D_p_s = D_p[xp,]
  xm = sample(1:dim(D_m)[1],0.2*dim(D_m)[1])
  D_m_s = D_m[xm,]
  for(j in which(degree!=0)){
    temp1 =temp2 = matrix(0,nrow = 1,ncol = emb_dim)
    if(length(D_p_s[which(D_p_s[, 1] == j),])==0 ||length(D_m_s[which(D_m_s[, 1] == j),])==0){
      break
    }
    else{
    for(k in t(D_p_s[which(D_p_s[,1]==j),])[,2]){
      temp1 = -mu[k,]* (exp( -mu[j,] %*% mu[k,])/(1+exp(-mu[j,] %*% mu[k,])))
    }

    for(k in t(D_m_s[which(D_m_s[,1]==j),])[,2]){
      temp2 = mu[k,]* (exp( mu[j,] %*% mu[k,])/(1+exp(mu[j,] %*% mu[k,])))
    }
    }
    mu[j] = mu[j]-0.1*(temp1+temp2)
  }
}

chat = kmeans(mu,2)$cluster
acc = max( mean((y+1)==chat), mean((-y+2)==chat ))
acc
















