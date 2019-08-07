#clear memory
rm( list=ls() )
library(dplyr)
library(sqldf)
setwd("D:\\Nomadgene_2018\\Teaching_2018\\summer capstone project 2019\\nVent\\Data");

##################################################################################################
d1 <-read.table("nVent_aggr_latest.csv",sep=",",header = TRUE, as.is=TRUE)
colnames(d1)[colnames(d1)=="Item.Nbr"] <-"Item_Nbr"
colnames(d1)[colnames(d1)=="DC.Branch"] <-"DC_Branch"
d3<-distinct(d1,Item_Nbr)

### output data structure 
d6 <- data.frame(matrix(ncol = 7, nrow = 0))
x <- c("Item_Nbr","DC_Branch" , "depl_flag","repl_flag_lt_ss","repl_flag_gt_ss","flag_idx","lead_time")
colnames(d6) <- x

df_v3<-data.frame()
df7<-data.frame()
df8<-data.frame() 

i<-1

for (i in 1:nrow(d3))
#for (i in 5:5)
{
  print(i)
  d2<-data.frame()
  d2<- sqldf(print(paste("select * from d1 where Item_Nbr =='",d3[i,],"'", sep="")))
  d4<-data.frame()
  d4<- sqldf("select distinct DC_Branch from d2")
  j<-1
  for (j in 1:nrow(d4))
  #for (j in 2:2)  
  {
    d5<-data.frame()
    d5<- sqldf(paste("select * from d2 where DC_Branch =='",d4[j,],"'", sep=""))
    ##d5[is.na(d5)] <- 0
    ### up to now .. the entire exercise was to build access to the 2 columns that need to be compared 
    v1<- as.numeric(d5[1,4:319])
    v2<-as.numeric(d5[3,4:319])
    v1[is.na(v1)] <- 0
    v2[is.na(v2)] <- 0
    mat<-matrix(c(v1,v2),nrow=316,ncol=2)
    if (all(mat == 0) | all(v1==0) | all(v2==0)) {
      print(paste("just landed into a zero matrix  ",j)) 
    }else {
    
    print(paste("i am fixing zeros for mat ",j)) 
    
    k<-1
    for(k in 1:nrow(mat)){
      if (mat[k,1] ==0 &   mat[k,2] ==0 & k >1){
         ## replace this row with values for previous row
        mat[k,1] <- mat[k-1,1]
        mat[k,2] <-mat[k-1,2]
      }
      
    }
    #### if matrix is full of zeroes ignore that set altogether 
    
    
    rownum <-1
    v3_depl <- vector(mode="numeric", length=0)
    v3_repl_lt_ss <- vector(mode="numeric", length=0)
    v3_repl_gt_ss <- vector(mode="numeric", length=0)
    flag_depl<-0
    
    for(rownum in 1:nrow(mat)) {
      
        
        
      
      if (mat[rownum,1] >=  mat[rownum,2]) {
          v3_depl<-rbind(v3_depl,0)
          v3_repl_lt_ss<-rbind(v3_repl_lt_ss,0)
          if (flag_depl==1){
            v3_repl_gt_ss<-rbind(v3_repl_gt_ss,1)
            flag_depl <-0
          } else {v3_repl_gt_ss<-rbind(v3_repl_gt_ss,0)}
        } else if (mat[rownum,1] <  mat[rownum,2]) {
          #append(v3,rownum,after=length(v3))
          v3_depl<-rbind(v3_depl,1)
          flag_depl<-1
        if(rownum !=1){
            if (mat[rownum,1] > mat[rownum-1,1]) {
              v3_repl_lt_ss<-rbind(v3_repl_lt_ss,1)
              v3_repl_gt_ss<-rbind(v3_repl_gt_ss,0)
            } else if  (mat[rownum,1] <= mat[rownum-1,1]) {
              v3_repl_lt_ss<-rbind(v3_repl_lt_ss,0)
              v3_repl_gt_ss<-rbind(v3_repl_gt_ss,0)
              
            } 
        } else {
          v3_repl_lt_ss<-rbind(v3_repl_lt_ss,0)
          v3_repl_gt_ss<-rbind(v3_repl_gt_ss,0)
          
        }
          
        } 
        
      
      
    }
    
    df_v3<-as.data.frame(cbind(v3_depl,v3_repl_lt_ss,v3_repl_gt_ss))
  #### put check here 
    if (all(df_v3 == 0))  {
      print(paste("landed in situation where no lead times exist",i,j))
    } else {
    
#if  (nrow(df_v3)!=0 ) {
    flag_idx <- vector(mode="numeric", length=nrow(df_v3))
    df_v3 <-cbind(df_v3,flag_idx)
    names(df_v3)<-c("v3_depl","v3_repl_lt_ss","v3_repl_gt_ss","flag_idx")
    
    ### compute flag index 
    
          blocksize<-0
          blockcounter <-0
          m<-1
          for(m in 1:nrow(df_v3)) {
                if(df_v3$v3_depl[m]==0 & df_v3$v3_repl_lt_ss[m]==0 & df_v3$v3_repl_gt_ss[m]==0) {
                      ##print(paste(" we are in 0 blocksize ",m))
                      if(blockcounter==1) {
                          blockcounter <-0
                          blocksize <-0
                      }
                  }else {
                              ## print(paste("landed in a non zero block size",m))
                               blockcounter <-1
                               blocksize<-blocksize + 1
                              if(blocksize ==1){
                                ### this is indicator of start of non zero block.
                                df_v3$flag_idx[m] <-m
                              }
                               if(df_v3$v3_repl_lt_ss[m]==1 ){
                                 df_v3$flag_idx[m] <-m
                               }
                               if (df_v3$v3_repl_gt_ss[m]==1 ) { 
                                  df_v3$flag_idx[m] <-m
                                  blocksize <-0
                                 
                                }
                      }
              }
              df7<-df_v3[df_v3$flag_idx > 0,]
              lead_time <- vector(mode="numeric", length=nrow(df7))
              df7 <-cbind(df7,lead_time)
              n<-1
              
              if (nrow(df7)==1) {
                    df7$lead_time[n] <-0
              }else {
                    for (n in 1:nrow(df7)){
                          if(df7$v3_repl_gt_ss[n]==0){
                             df7$lead_time[n] <- df7$flag_idx[n+1]-df7$flag_idx[n]
                            
                          }else {df7$lead_time[n] <-0}
                    }
              }
              df7 <-na.omit(df7)
              df8<-cbind(d5[1,2],d5[1,1],df7)
              names(df8)<- c("Item_Nbr","DC_Branch" , "depl_flag","repl_flag_lt_ss","repl_flag_gt_ss","flag_idx","lead_time")
                
                d6<-rbind(d6,df8)
                
                #write.table(df8, "nvent_lead_times_all_items_run2.csv", sep = ",", col.names = !file.exists("nvent_lead_times_all_items_run2.csv"), append = T) 
              
              
    
  }
    
    }
  }
}
write.table(d6, "nvent_complete_lead_times_RUN333333.csv", sep=",",row.names=FALSE, quote=FALSE)

#######  Expected Lead time model... 

d10 <-read.table("nVent_avlbl_sales_sstk_latest.csv",sep=",",header = TRUE, as.is=TRUE)
colnames(d10)[colnames(d10)=="Item.Nbr"] <-"Item_Nbr"
colnames(d10)[colnames(d10)=="DC.Branch"] <-"DC_Branch"

d30<-distinct(d10,Item_Nbr)
### output data structure 
d60 <- data.frame(matrix(ncol = 5, nrow = 0))
x <- c("Item_Nbr","DC_Branch" ,"Mean_Available_qty","Mean_sales_qty","mean_sstk_qty")
colnames(d60) <- x


df80<-data.frame() 

i<-1

for (i in 1:nrow(d30))
  #for (i in 5:5)
{
  print(i)
  d20<-data.frame()
  d20<- sqldf(print(paste("select * from d10 where Item_Nbr =='",d30[i,],"'", sep="")))
  d40<-data.frame()
  d40<- sqldf("select distinct DC_Branch from d20")
  j<-1
  for (j in 1:nrow(d40))
    #for (j in 2:2)  
  {
    d50<-data.frame()
    d50<- sqldf(paste("select * from d20 where DC_Branch =='",d40[j,],"'", sep=""))
    df80<-cbind(d50[1,1],d50[1,2],d50[1,4],d50[2,4],d50[3,4])
    names(df80)<- c("Item_Nbr","DC_Branch" ,"Mean_Available_qty","Mean_sales_qty","mean_sstk_qty")
    d60<-rbind(d60,df80)
    
    
  }
}
names(d60)<- c("Item_Nbr","DC_Branch" ,"Mean_Available_qty","Mean_sales_qty","mean_sstk_qty")  

d20 <-read.table("nVent_mean_lead_times.csv",sep=",",header = TRUE, as.is=TRUE)



join_string <- "select
d60.*
, d2.lead_time_Mean
from d60
inner join d20
on d60.Item_Nbr = d20.Item_Nbr and 
d60.DC_Branch = d20.DC_Branch"

d70 <- sqldf(join_string,stringsAsFactors = FALSE)

write.table(d70, "nvent_mean_qty_lead_times.csv", sep=",",row.names=FALSE, quote=FALSE)

d70$Mean_Available_qty <-as.numeric(d70$Mean_Available_qty)
d70$mean_sstk_qty <-as.numeric(d70$mean_sstk_qty)

mdl_lt<-lm(lead_time_Mean ~  Mean_Available_qty + mean_sstk_qty +  as.factor(DC_Branch), 
           data= d70 )

mdl_lt1<-lm(lead_time_Mean ~  Mean_Available_qty + mean_sstk_qty ,data= d70 )

mdl_lt2<-lm(lead_time_Mean ~  as.factor(DC_Branch), data= d70 )
