#' @title Calculate the center of K-mean cluster using R
#' @description Calculate the center of K-mean cluster using R
#' @param data a data matrix or a dist object
#' @param k the number of cluster
#' @param max.iter Maximum number of iterations for clustering. The default value is 10. 
#' @return Cluster center for each class
#' @examples
#' \dontrun{
#' data<-iris[,1:2]
#' k<-3
#' max.iter<-10
#' z<-My_kmeans(data,k,max.iter)
#' z
#' }
#' @export
My_kmeans <- function(data,k,max.iter=10){
  
  rows <- nrow(data) 
  cols <- ncol(data) 
  
  within <- matrix(0,nrow=k,ncol=1) 
  between <- 0
  iter = 0
  
  #定义indexMatrix矩阵,第一列为每个数据所在的类，第二列为每个数据到其类中心的距离
  indexMatrix <- matrix(0,nrow=rows,ncol=2) 
  
  centers <- matrix(0,nrow=k,ncol=cols) 
  randSeveralInteger <- as.vector(sample(1:rows,size=k))
  #通过生成随机数的方式，得到初始的聚类中心
  for(i in 1:k){
    indexMatrix[randSeveralInteger[i],1] <- i
    centers[i,] <- data[randSeveralInteger[i],]
    centers <- matrix(centers,k,cols)
  }
  changed = TRUE 
  
  while(changed){ 
    
    if(iter >= max.iter)
      break
    
    changed=FALSE
    
    #对每一个数据，计算其到各个类中心的距离，并将其划分到距离最近的类
    for(i in 1:rows){ 
      initialDistance <- 10000 
      previousCluster <- indexMatrix[i,1]
      
      #遍历所有的类，将该数据划分到距离最近的类
      for(j in 1:k){ 
        currentDistance <- (sum((data[i,]-centers[j,])^2))^0.5
        if(currentDistance < initialDistance){
          initialDistance <- currentDistance 
          indexMatrix[i,1] <- j 
          indexMatrix[i,2] <- currentDistance 
        } 
      }
      
      #如果该数据所属的类发生了变化，则将changed设为TRUE，算法继续
      if(previousCluster!=indexMatrix[i,1]) 
        changed=TRUE
    }
    
    #重新计算类中心
    for(m in 1:k){
      clusterMatrix <- data[indexMatrix[,1]==m,] 
      clusterMatrix <- as.matrix(clusterMatrix)
      if(nrow(clusterMatrix)>0){ 
        centers[m,] <- colMeans(clusterMatrix) 
      } 
      else{
        centers[m,] <- centers[m,] 
      }    
    }
    iter = (iter+1)
  }
  return(centers)
}