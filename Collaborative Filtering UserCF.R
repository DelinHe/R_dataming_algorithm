 
rating<-read.csv("C:\\MOVIE_Data\\ml-100k\\u1.base",header=T,sep=",")
# 标识user与subject的索引
user = sort(unique(rating$UserID))
movie = sort(unique(rating$MovieID))
uidx = match(rating$UserID, user)
iidx = match(rating$MovieID, movie)
# 从二元组构造收藏矩阵
M = matrix(NA,nrow=length(user), ncol=length(movie) 
           , dimnames = list(
             user=user,
             item=movie    )
)
index = cbind(uidx, iidx)
M[index]=rating[,3]
#转成评分
library("recommenderlab")
M_r<-as(M, "realRatingMatrix")
r_b <- binarize(M_r, minRating=1)
M_b <- as(r_b, "matrix")
dimnames(M_b) <- list(user=user,item=movie)

#构造user 和item的余弦相似度矩阵
M_U= matrix(0,nrow=length(user), ncol=length(user),dimnames = list(
  user=user,user=user)) 



#用户倒排表算
item_num<-rowSums(M_b)
for(i in 1:length(movie))
{    
  z<-match(as.integer(names(M_b[M_b[,i]>0,i])), user)
  x<-expand.grid(z,z)       
  x<-as.matrix(x[x[,1]!=x[,2],]) #计算倒排表中的用户的两两之间
  M_U[x]=M_U[x]+1
  #通过log评分惩罚流行物品
  #M_U[x]=M_U[x]+1/log(1+length(z))
}
#交集除以并  即计算余弦相似度
for(i in 1:length(user))  
  M_U[i,]= sqrt((M_U^2)[i,]/(item_num[i]*item_num) )
 
#userCF算法
M_U_TOP = apply(M_U, 1, FUN=sort, decreasing=TRUE, index.return=TRUE)
k = 10
# 取出前K个用户相似度评分最大的用户
Top_user = lapply(M_U_TOP, FUN=function(r) 
  return(list(Top_user=user[r$ix[1:k]],Top_sim=r$x[1:k])))
#对item i有过行为的用户集合
M_U_Item<- apply(M_b, 2, FUN=function(r) 
  return(match(as.integer(dimnames(M_b[r>0,])$user), user)))

#为用户推荐未有过行为的item
#同兴趣的K个用户集合和相似度值
#S<-Top_user[[u]]  S$Top_user  S$Top_sim
M_recom<-data.frame()
for(u in user)
{   rank<-double(length(movie))
    rank<-lapply(1:length(movie), FUN=function(i) 
      return(ifelse(M_b[u,i]==0,rank_score_U_CF(Top_user[[u]],M_U_Item[[i]],M_U[u,],rank[i]),11111.00)) )  
    M_recom<-rbind(M_recom,rank)
}
#对所有用户计算其推荐的ITEM评分 替换上面的脚本  速度进步还行
M_recom<-lapply(user,FUN=function(u) return(recom(u)))
#将推荐结果的LIST转化成数据框 
result<-data.frame()
for(i in user )
{
  z1<-M_recom[[i]]$user
  z2<-M_recom[[i]]$score
  z2<-as.data.frame(z2)
  rownames(z2)<-c(z1)
  colnames(z2)<-movie
  result<-rbind(result,z2)
}

#依次计算未访问过的ITEM的推荐评分
#计算单个item的评分 封装成函数 并行化操作
#输入 j S$Top_user 
rank_score_U_CF<-function(S,M_U_Item,M_U,rank)
{
  for(u in S$Top_user )
  {
    if( u %in% M_U_Item)
    {  rank=rank+M_U[u]  }    
  }
  return(rank)
}


 

 





 


 
 

