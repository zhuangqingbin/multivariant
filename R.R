a <- matrix(c(1,2,3,4,5,6,7,8,9,10,11,12),byrow=T,3,4)
a
cov(a)


discriminiant.distance <-function
(TrnX1, TrnX2,TstX=NULL,var.equal=FALSE){
if (is.null(TstX)==TRUE) TstX<-rbind(TrnX1,TrnX2)
if (is.vector(TstX)==TRUE) TstX<-t(as.matrix(TstX))
else if (is.matrix(TstX) != TRUE)
  TstX <- as.matrix(TstX)
if (is.matrix(TrnX1) != TRUE) TrnX1 <- as.matrix(TrnX1)
if (is.matrix(TrnX2) != TRUE) TrnX2 <- as.matrix(TrnX2)
nx <- nrow(TstX)
blong <- matrix(rep(0, nx), nrow=1, byrow=TRUE,
                  dimnames=list("blong", 1:nx))
mu1 <- colMeans(TrnX1); mu2 <- colMeans(TrnX2)
if (var.equal == TRUE || var.equal == T){
  S <- var(rbind(TrnX1,TrnX2))
  w <- mahalanobis(TstX, mu2, S)- mahalanobis(TstX, mu1, S)
  }
else{
  S1<-var(TrnX1); S2<-var(TrnX2)
  w<- mahalanobis(TstX, mu2, S2)-mahalanobis(TstX, mu1, S1)
}
for (i in 1:nx){
  if (w[i]> 0)
    blong[i]<- 1
  else
    blong[i]<- 2
  }
  blong
}

classX1<-data.frame(
  x1=c(6.60, 6.60, 6.10, 6.10, 8.40, 7.2, 8.40, 7.50,
       7.50, 8.30, 7.80, 7.80),
  x2=c(39.00,39.00, 47.00, 47.00, 32.00, 6.0, 113.00, 52.00,
       52.00,113.00,172.00,172.00),
  x3=c(1.00, 1.00, 1.00, 1.00, 2.00, 1.0, 3.50, 1.00,
       3.50, 0.00, 1.00, 1.50),
  x4=c(6.00, 6.00, 6.00, 6.00, 7.50, 7.0, 6.00, 6.00,
       7.50, 7.50, 3.50, 3.00),
  x5=c(6.00, 12.00, 6.00, 12.00, 19.00, 28.0, 18.00, 12.00,
       6.00, 35.00, 14.00, 15.00),
  x6=c(0.12, 0.12, 0.08, 0.08, 0.35, 0.3, 0.15, 0.16,
       0.16, 0.12, 0.21, 0.21),
  x7=c(20.00,20.00, 12.00, 12.00, 75.00, 30.0, 75.00, 40.00,
       40.00,180.00, 45.00, 45.00)
)

classX2<-data.frame(
  x1=c(8.40, 8.40, 8.40, 6.3, 7.00, 7.00, 7.00, 8.30,
       8.30, 7.2, 7.2, 7.2, 5.50, 8.40, 8.40, 7.50,
       7.50, 8.30, 8.30, 8.30, 8.30, 7.80, 7.80),
  x2=c(32.0 ,32.00, 32.00, 11.0, 8.00, 8.00, 8.00,161.00,
       161.0, 6.0, 6.0, 6.0, 6.00,113.00,113.00, 52.00,
       52.00, 97.00, 97.00,89.00,56.00,172.00,283.00),
  x3=c(1.00, 2.00, 2.50, 4.5, 4.50, 6.00, 1.50, 1.50,
       0.50, 3.5, 1.0, 1.0, 2.50, 3.50, 3.50, 1.00,
       1.00, 0.00, 2.50, 0.00, 1.50, 1.00, 1.00),
  x4=c(5.00, 9.00, 4.00, 7.5, 4.50, 7.50, 6.00, 4.00,
       2.50, 4.0, 3.0, 6.0, 3.00, 4.50, 4.50, 6.00,
       7.50, 6.00, 6.00, 6.00, 6.00, 3.50, 4.50),
  x5=c(4.00, 10.00, 10.00, 3.0, 9.00, 4.00, 1.00, 4.00,
       1.00, 12.0, 3.0, 5.0, 7.00, 6.00, 8.00, 6.00,
       8.00, 5.00, 5.00,10.00,13.00, 6.00, 6.00),
  x6=c(0.35, 0.35, 0.35, 0.2, 0.25, 0.25, 0.25, 0.08,
       0.08, 0.30, 0.3, 0.3, 0.18, 0.15, 0.15, 0.16,
       0.16, 0.15, 0.15, 0.16, 0.25, 0.21, 0.18),
  x7=c(75.00,75.00, 75.00, 15.0,30.00, 30.00, 30.00, 70.00,
       70.00, 30.0, 30.0, 30.0,18.00, 75.00, 75.00, 40.00,
       40.00,180.00,180.00,180.00,180.00,45.00,45.00)
)
discriminiant.distance(classX1, classX2, var.equal=TRUE)





distinguish.distance <- function
(TrnX, TrnG, TstX = NULL, var.equal = FALSE){
  if ( is.factor(TrnG) == FALSE){
    mx <- nrow(TrnX); mg <- nrow(TrnG)
    TrnX <- rbind(TrnX, TrnG)
    TrnG <- factor(rep(1:2, c(mx, mg)))
  }
  if (is.null(TstX) == TRUE) TstX <- TrnX
  if (is.vector(TstX) == TRUE) TstX <- t(as.matrix(TstX))
  else if (is.matrix(TstX) != TRUE)
    TstX <- as.matrix(TstX)
  if (is.matrix(TrnX) != TRUE) TrnX <- as.matrix(TrnX)
  nx <- nrow(TstX)
  blong <- matrix(rep(0, nx), nrow=1,
                  dimnames=list("blong", 1:nx))
  g <- length(levels(TrnG))
  mu <- matrix(0, nrow=g, ncol=ncol(TrnX))
  for (i in 1:g)
    mu[i,] <- colMeans(TrnX[TrnG==i,])
  D <-matrix(0, nrow=g, ncol=nx)
  if (var.equal == TRUE || var.equal == T){
    for (i in 1:g)
      D[i,] <- mahalanobis(TstX, mu[i,], var(TrnX))
  }
  else{
    for (i in 1:g)
      D[i,] <- mahalanobis(TstX, mu[i,], var(TrnX[TrnG==i,]))
  }
  for (j in 1:nx){
    dmin <- Inf
    for (i in 1:g)
      if (D[i,j] < dmin){
        dmin <- D[i,j]; blong[j] <- i
      }
  }
  percentage=sum(as.numeric(blong)==as.numeric(TrnG))/length(TrnG)
  location=which(as.numeric(blong)!=as.numeric(TrnG))
  list1 <- list(blong=blong,percentage=percentage,location=location)
  list1
}

X<-iris[,1:4]
G<-gl(3,50)
distinguish.distance(X,G)
