n = 1000
k = 2
pi = c(0.5,0.5)
c = 15
lambda = 0.8
num_paths =1
length_path = 10
emb_dim = 5
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

G = matrix(0,ncol = n,nrow = n)
for(i in 1:n){
  for (j in i:n){
    temp1 = length(which(D_p==c(i,j)))
    temp2 = length(which(D_m==c(i,j)))
    if(temp1*temp2 != 0){
      G[i,j]=G[j,i]=log(temp1/temp2)
    }
    else{
      G[i,j]=G[j,i]=0
    }
  }
}
svd_c = svd(G)
u = svd_c$d
dim = sum(u>5)
mu = svd_c$u[,1:3]


k = 2
chat = kmeans(mu,k,iter.max = 1000)$cluster
acc = max( mean((y+1)==chat), mean((-y+2)==chat ))
acc

