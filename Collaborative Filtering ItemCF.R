 
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

#构造item 和item的余弦相似度矩阵
M_I= matrix(0,nrow=length(movie), ncol=length(movie),dimnames = list(
  item=movie,item=movie)) 

#Item倒排表算
user_num<-colSums(M_b)
for(i in 1:length(user))
{  
  z<-match(as.integer(names(M_b[i,M_b[i,]>0])) , movie)
  x<-expand.grid(z,z)       
  x<-as.matrix(x[x[,1]!=x[,2],]) #计算倒排表中的用户的两两之间
  M_I[x]=M_I[x]+1   
  #M_I[x]=M_I[x]+1/log(1+length(z))  # IUF  惩罚活跃用户 对物品相似度的影响
  
}
#交集除以并  即计算余弦相似度
for(i in 1:length(movie))  
  M_I[i,]= sqrt((M_I^2)[i,]/(user_num[i]*user_num) ) 

#将物品相似度矩阵归一化  wij/max(wj)
M_I_norm<-M_I
for( i in length(movie))
  M_I_norm[,1]=M_I[,1]/max(M_I[,1])  #没法用apply  返回的全是list

#item CF算法
M_I_TOP = apply(M_I, 1, FUN=sort, decreasing=TRUE, index.return=TRUE)
k = 30
# 取出前K个item相似度评分最大的item
Top_item = lapply(M_I_TOP, FUN=function(r) 
  return(list(Top_item=movie[r$ix[1:k]],Top_sim=r$x[1:k]))) # ix返回的不是item而是序列
#用户u喜欢的ITEM集合
M_I_USER<- apply(M_b, 1, FUN=function(r) 
  #return(match(as.integer(dimnames(M_b[,r>0])$item), movie)))
  return(as.integer(dimnames(M_b[,r>0])$item)))

#为用户推荐相似的item
 
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

#并行计算ITEM推荐评分
recom<-function(u)
{ 
  rank<-double(length(movie))
  rank<-lapply(1:length(movie), FUN=function(i) 
  return(ifelse(M_b[u,i]==0,rank_score_I_CF(Top_item[[i]],M_I_USER[[u]],M_I[i,],rank[i]) ,999999.00)) )  
  res<-list(u,rank) 
  names(res)<-c("user","score")
  return(res)
}
 
#计算单个item的评分 封装成函数 并行化操作
rank_score_I_CF<-function(Top,M_I_USER,M_I,rank)
{
  for(j in Top$Top_item )
  {
    if( j %in% M_I_USER)
    {  rank=rank+M_I[j]
       #rank$reason[i]$reason_item[j]=j
    }    
  }
  return(rank)
}
 


 


 
 


