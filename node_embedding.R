n = 1000
k = 2
pi = c(0.5,0.5)
c = 15
lambda = 0.8
num_paths =1
length_path = 10
<<<<<<< HEAD
emb_dim = 3
=======
emb_dim = 5
>>>>>>> 9344299d2361498fd67a17820becb0a1e7ed14fb
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

#neighbor
neighbor = matrix(0,nrow = n,ncol = n)
for(i in 1:n){
  neighbor[i,1]=length(which(A[i,]==1))
  if(neighbor[i,1] !=0){
    for(j in 2:(1+neighbor[i,1])){
      neighbor[i,j]=which(A[i,]==1)[j-1]
    }
  }
}

#Random walk
for(i in which(degree!=0)){
  start = i
  rw = NULL
  rw = c(rw,start)
  for(j in 1:(length_path-1)){
    if(neighbor[start,1]>1){
      end = sample(neighbor[start,2:(neighbor[start,1]+1)],1)
    }
    else{
      end = neighbor[start,2]
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

<<<<<<< HEAD
#optimize
rate = 0.005
=======
#Optimize
rate = 0.01
>>>>>>> 9344299d2361498fd67a17820becb0a1e7ed14fb
mu = matrix(rnorm(n = n*emb_dim,mean = 0,sd = 1),nrow = n,ncol = emb_dim)
for (i in 1:100){
  xp = sample(1:dim(D_p)[1],0.2*dim(D_p)[1])
  xm = sample(1:dim(D_m)[1],0.2*dim(D_m)[1])
  temp1 =temp2 = matrix(0,nrow = n,ncol = emb_dim)
  for(j in xp){
    ii = D_p[j,][1]
    jj = D_p[j,][2]
    temp1[ii,] = temp1[ii,] + (exp( -mu[ii,] %*% mu[jj,])/(1+exp(-mu[ii,] %*% mu[jj,]))) %*% (-mu[jj,])
    temp1[jj,] = temp1[jj,] + (exp( -mu[jj,] %*% mu[ii,])/(1+exp(-mu[jj,] %*% mu[ii,]))) %*% (-mu[ii,])
  }
  for(j in xm){
    ii = D_m[j,][1]
    jj = D_m[j,][2]
    temp2[ii,] = temp2[ii,] + (exp( mu[ii,] %*% mu[jj,])/(1+exp(mu[ii,] %*% mu[jj,]))) %*% (mu[jj,])
    temp2[jj,] = temp2[jj,] + (exp( mu[jj,] %*% mu[ii,])/(1+exp(mu[jj,] %*% mu[ii,]))) %*% (mu[ii,])
  }
  mu = mu - rate*(temp1+temp2)
}

chat = kmeans(mu,2)$cluster
acc = max( mean((y+1)==chat), mean((-y+2)==chat ))
acc















