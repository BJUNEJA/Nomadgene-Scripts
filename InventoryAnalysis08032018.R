rm(list=ls())
library(dplyr)
library(ggplot2)
library(reshape2)
library(factoextra)

setwd("D:\\Nomadgene_2018\\Working_Papers_2018\\Ecolab-Inventory\\Data")
nalco <-tbl_df(read.csv("IPT-Long.csv", sep=",", header=TRUE, as.is=TRUE, fill=TRUE))
nlc_2018<- tbl_df(read.csv("NLC_QTY_CONSUMPTION_2018.csv",sep=",", header=TRUE, as.is=TRUE, fill=TRUE))

#### 

np<- nalco %>%                    
  filter(!is.na(Plant_ID)) %>%  
  filter(Cycle=="July") %>%
  group_by(Plant_ID) %>%          
  summarise(Material_Count = n_distinct(Material)) %>% 
  arrange(desc(Material_Count)) 
  
write.table(np, "plant-uniqueproduct.csv", sep=",",row.names=FALSE, quote=FALSE)

np1<- filter(np,Material_Count < 10)

np2<-   nalco %>%
        filter(Inventory ==0,Cycle=="July") %>%
        group_by(Plant_ID) %>%
        summarise(Material_Count = n_distinct(Material)) %>%
        arrange(desc(Material_Count))
        View(np2)
        write.table(np2, "plant-material_zero_inventory.csv", sep=",",row.names=FALSE, quote=FALSE)
        

###### You can expand this to get list of materials by each plant that carry zero inventory. 
np3<- nalco %>%
  filter(Plant_ID == "106",Cycle=="July") %>%
  filter(Inventory ==0) %>%
  distinct(Material) 

View(np3)
####################################

##############################################
np4<- nalco %>%
  filter(Plant_ID == "106",Cycle=="July") %>%
  group_by(Plant_ID,Inventory) %>%
  distinct(Material) %>%
  arrange(desc(Inventory)) 
View(np4)
####################################
##############################################



np5<- nalco %>%
  filter(Plant_ID == "106",Cycle=="August") %>%
  group_by(Plant_ID,Inventory) %>%
  distinct(Material) %>%
  arrange(desc(Inventory)) 
View(np5)

np6<- nalco %>%
  filter(Plant_ID == "140",Cycle=="August") %>%
  group_by(Plant_ID,Inventory) %>%
  distinct(Material) %>%
  arrange(desc(Inventory)) 
View(np6)


####################################

#### Is there an overlap between products across plants. 
common <- intersect(np5$Material, np6$Material)  
dim(common)

####################################
### Lets get consumption data. This will need some massaging before it can be consumed. Isn't it irony 
## that consumption data is not consumable :-).. fuck it 
consumption <-tbl_df(read.csv("NALCO-Inventory-Consumption1.csv", sep=",", header=TRUE, as.is=TRUE, fill=TRUE))

## The desired format for this data is:  
###<Plant 1><Material 1><Month 1><Consumption_KG>
###<Plant 1><Material 1><Month 2><Consumption_KG>
###......  Its easy , just need a little coding.  

### the magic is done by reshape2 library

mdata <- melt(consumption, id=c("Plant","Material"))
write.table(mdata, "nalco-consumption-transposed.csv", sep=",",row.names=FALSE, quote=FALSE)

consumption_T <-tbl_df(read.csv("NALCO-Inventory-Consumption_T_FINAL.csv", sep=",", header=TRUE, as.is=TRUE, fill=TRUE))
##################################
### Next step : intersect consumption with Inventory and then compute inventory turn over rate Inventory/consumption 





#################################################################################
## k-Means clustering on ECL Finished goods data
setwd("D:\\Nomadgene_2018\\Working_Papers_2018\\Ecolab-Inventory\\Data")
ecl_2018<- tbl_df(read.csv("ECL_QTY_CONSUMPTION_Reordered_2018.csv",sep=",", header=TRUE, as.is=TRUE, fill=TRUE))
ecl_2018$Qty<-log(ecl_2018$Qty)
#ecl_2018$Standard_cost <-log(nlc_2018$Standard_cost)
##### Unscaled version
ggplot(ecl_2018, aes(Standard_cost, Qty, color = Plant)) + geom_point()
#####  Scaled Version 
ecl_2018_scaled <-scale(ecl_2018[2:3])
ecl_2018_scaled <-cbind(ecl_2018_scaled, ecl_2018[1])
ecl_2018_scaled <-cbind(ecl_2018_scaled, ecl_2018[4:7])
### reorder columns for easy read.. 
ecl_2018_scaled <-ecl_2018_scaled[c(3,1,2,4,5,6,7)]
ggplot(ecl_2018_scaled, aes(Standard_cost, Qty, color = Plant)) + geom_point()
###################Unscaled Clustering #########################################
set.seed(20)
km.ecl.unscaled <- kmeans(ecl_2018[, c("Qty","Standard_cost")], 3, nstart = 20)
fviz_cluster(km.ecl.unscaled, ecl_2018[,2:3],palette = "Set2", geom = "point", ggtheme = theme_minimal())
#fviz_cluster(cluster1, nlc_2018[,2:3], geom = "point",ellipse.type = "norm")
###################Scaled Clustering #########################################
set.seed(20)
km.ecl.scaled <- kmeans(ecl_2018_scaled[, c("Qty","Standard_cost")], 3, nstart = 20)
fviz_cluster(km.ecl.scaled, ecl_2018_scaled[,2:3],palette = "Set2", geom = "point", ggtheme = theme_minimal())
ecl_cluster<-km.ecl.scaled$cluster
ecl_2018_scaled <- cbind(ecl_2018_scaled ,ecl_cluster)

#fviz_cluster(km.scaled, nlc_2018_scaled[,2:3], geom = "point",ellipse.type = "norm")

###For now lets do it hard way AKA a For loop > create  a column vector called ABC_Classification  
i<-1
j<-length(ecl_2018_scaled$ecl_cluster)
abc_classification<-""

for (i in 1:j) {
  #print (i)
  
  if (ecl_2018_scaled$ecl_cluster[i] == 3) {
    abc_classification[i] <- "B" }    ##### 20% OF ITEMS 
  else if(ecl_2018_scaled$ecl_cluster[i] == 1) {
    abc_classification[i] <- "C" }   ### 70% OF ITEMS 
  else if(ecl_2018_scaled$ecl_cluster[i] == 2) {
    abc_classification[i] <- "A"  }  ####10% OF ITEMS OR MORE 
  
 
  
}

ecl_2018_scaled <-cbind(ecl_2018_scaled, abc_classification) 




#################################################################################
#ecl_2018_scaled <-ecl_2018_scaled[-9]

ecl_gc_legacy <- tbl_df(read.csv("GC_Legacy_ELC.csv",sep=",", header=TRUE, as.is=TRUE, fill=TRUE))

### Our next step is to draw comparison between GC Legacy and Our ABC- Classification 
#### One idea would be to:
## Loop it... 
i<-1
j<-length(ecl_2018_scaled$ecl_cluster)
abc_classification_legacy_ecl<-""

for (i in 1:j) {
  #print (i)
  
  prod <-ecl_2018_scaled$Material[i]
  plant <-ecl_2018_scaled$Plant[i]
  
  tmp<- filter(ecl_gc_legacy,(Plant==plant & Products==prod ))
  if (length(tmp$Volume_ABC)==0) {
    
    abc_classification_legacy_ecl[i] <-"NA"
    print(abc_classification_legacy_ecl[i])
  }   else      {
    abc_classification_legacy_ecl[i] <-tmp$Volume_ABC
    print(abc_classification_legacy_ecl[i])
    
  }
   
  
}

ecl_2018_scaled <-cbind(ecl_2018_scaled, abc_classification_legacy_ecl) 


i<-1
j<-length(ecl_2018_scaled$ecl_cluster)
abc_match_minevslegacy<-""

for (i in 1:j) {
  #print (i)
  
if (ecl_2018_scaled$abc_classification[i] == ecl_2018_scaled$abc_classification_legacy_ecl[i]) {
        abc_match_minevslegacy[i] <- 1 
    }  else  {
        abc_match_minevslegacy[i] <-0
 }
  
  
}
ecl_2018_scaled <-cbind(ecl_2018_scaled, abc_match_minevslegacy) 

match<-filter(ecl_2018_scaled,ecl_2018_scaled$abc_match_minevslegacy==1)
nomatch<-filter(ecl_2018_scaled,ecl_2018_scaled$abc_match_minevslegacy==0) 

perc<- (length(match)/length(match)+length(nomatch))*100
#### Lets save it for now... 
write.table(ecl_2018_scaled, "ecl_2018_scaled_07232018_1833.csv", sep=",",row.names=FALSE, quote=FALSE)

###### ABC Classification using standard theory approach 
##Determine the annual usage for each item





ecl_2018_raw<- tbl_df(read.csv("ECL_QTY_CONSUMPTION_Reordered_2018.csv",sep=",", header=TRUE, as.is=TRUE, fill=TRUE))

##Multiply the annual usage of each item by its cost to get its total annual usage in monetary unit
ecl_2018_raw <- ecl_2018_raw %>% mutate(Usage_RMB = Qty * Standard_cost)

ecl_2018_raw <- ecl_2018_raw %>% mutate(Usage_RMB_amount = Qty * Amount)

##List the items according to their annual usage in monetary unit in descending order
ecl_2018_raw_usage_desc<- ecl_2018_raw %>%                    
               arrange(desc(Usage_RMB_amount))

##Calculate the cumulative annual usage in monetary unit

cum_usage_amount<-cumsum(ecl_2018_raw_usage_desc$Usage_RMB_amount)
ecl_2018_raw_usage_desc <-cbind(ecl_2018_raw_usage_desc,cum_usage_amount)
sum_usage_amount<-sum(ecl_2018_raw_usage_desc$Usage_RMB_amount)
tot_perc_usage_amount<-(ecl_2018_raw_usage_desc$Usage_RMB_amount /sum_usage_amount)*100
ecl_2018_raw_usage_desc <-cbind(ecl_2018_raw_usage_desc,tot_perc_usage_amount)
cum_usage_perc_amount <-(ecl_2018_raw_usage_desc$cum_usage_amount /sum_usage_amount)*100
ecl_2018_raw_usage_desc <-cbind(ecl_2018_raw_usage_desc,cum_usage_perc_amount)


sum_usage<-sum(ecl_2018_raw_usage_desc$Usage_RMB)
tot_perc_usage_raw<-(ecl_2018_raw_usage_desc$Usage_RMB /sum_usage)*100
ecl_2018_raw_usage_desc <-cbind(ecl_2018_raw_usage_desc,tot_perc_usage_raw)


cum_usage_perc <-(ecl_2018_raw_usage_desc$cum_usage /sum_usage)*100
ecl_2018_raw_usage_desc <-cbind(ecl_2018_raw_usage_desc,cum_usage_perc)


write.table(ecl_2018_raw_usage_desc, "ecl_cumulative_usage_amount_descending.csv", sep=",",row.names=FALSE, quote=FALSE)


## calculate the cumulative percentage of items. 
tot_qty<-sum(ecl_2018_raw_usage_desc$Qty)
cum_qty<- cumsum(ecl_2018_raw_usage_desc$Qty)
ecl_2018_raw_usage_desc <-cbind(ecl_2018_raw_usage_desc,cum_qty)
ecl_2018_raw_usage_desc <-cbind(ecl_2018_raw_usage_desc,perc_qty)

##Examine the annual usage distribution
x <- log(ecl_2018_raw_usage_desc$Usage_RMB_amount)
h<-hist(x, breaks=100, col="red", xlab="Usage in RMB", 
        main="Usage Histogram with Normal curve") 
xfit<-seq(min(x),max(x),length=40) 
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x)) 
yfit <- yfit*diff(h$mids[1:2])*length(x) 
lines(xfit, yfit, col="blue", lwd=2)

##group the items into three classes A, B and C. 


###https://logisticaudit.wordpress.com/tag/abcxyz-inventory-control/
ecl_2018_raw_usage_desc<-ecl_2018_raw_usage_desc[,-14]
i<-1
j<-length(ecl_2018_raw_usage_desc$cum_usage_perc_amount)
abc_classification_LA<-""

for (i in 1:j) {
  #print (i)
  
  if (ecl_2018_raw_usage_desc$cum_usage_perc_amount[i] <= 81) {
    abc_classification_LA[i] <- "A" }   
  else if(ecl_2018_raw_usage_desc$cum_usage_perc_amount[i] > 81 && ecl_2018_raw_usage_desc$cum_usage_perc_amount[i] <= 95) {
    abc_classification_LA[i] <- "B" }   
  else if(ecl_2018_raw_usage_desc$cum_usage_perc_amount[i] > 95) {
    abc_classification_LA[i] <- "C"   }  
  
  
}

ecl_2018_raw_usage_desc <-cbind(ecl_2018_raw_usage_desc, abc_classification_LA) 


ecl_gc_legacy <- tbl_df(read.csv("GC_Legacy_ELC.csv",sep=",", header=TRUE, as.is=TRUE, fill=TRUE))

### Our next step is to draw comparison between GC Legacy and LA ABC- Classification 
#### One idea would be to:
## Loop it... 
i<-1
j<-length(ecl_2018_raw_usage_desc$abc_classification_LA)
abc_classification_legacy_ecl<-""

for (i in 1:j) {
  #print (i)
  
  prod <-ecl_2018_raw_usage_desc$Material[i]
  plant <-ecl_2018_raw_usage_desc$Plant[i]
  
  tmp<- filter(ecl_gc_legacy,(Plant==plant & Products==prod ))
  if (length(tmp$Volume_ABC)==0) {
    
    abc_classification_legacy_ecl[i] <-"NA"
    print(abc_classification_legacy_ecl[i])
  }   else      {
    abc_classification_legacy_ecl[i] <-tmp$Volume_ABC
    print(abc_classification_legacy_ecl[i])
    
  }
  
  
}

ecl_2018_raw_usage_desc <-cbind(ecl_2018_raw_usage_desc, abc_classification_legacy_ecl) 


#### Append --Kmeans output here as well 

i<-1
j<-length(ecl_2018_raw_usage_desc$abc_classification_LA)
abc_classification_km_ecl<-""
tmp<-""
for (i in 1:j) {
  #print (i)
  
  prod <-ecl_2018_raw_usage_desc$Material[i]
  plant <-ecl_2018_raw_usage_desc$Plant[i]
  
  tmp<- filter(ecl_2018_scaled,(Plant==plant & Material==prod ))
  if (length(tmp$abc_classification)==0) {
    
    abc_classification_km_ecl[i] <-"NA"
    print(abc_classification_km_ecl[i])
  }   else      {
    abc_classification_km_ecl[i] <-as.character(tmp$abc_classification)
    print(abc_classification_km_ecl[i])
    
  }
  
  
}

ecl_2018_raw_usage_desc <-cbind(ecl_2018_raw_usage_desc, abc_classification_km_ecl) 





#### FND MATCH BETWEEN LA and Legacy 
i<-1
j<-length(ecl_2018_raw_usage_desc$abc_classification_LA)
abc_match_LAvslegacy<-""

for (i in 1:j) {
  #print (i)
  
  if (ecl_2018_raw_usage_desc$abc_classification_LA[i] == ecl_2018_raw_usage_desc$abc_classification_legacy_ecl[i]) {
    abc_match_LAvslegacy[i] <- 1 
  }  else  {
    abc_match_LAvslegacy[i] <-0
  }
  
  
}
ecl_2018_raw_usage_desc <-cbind(ecl_2018_raw_usage_desc, abc_match_LAvslegacy) 


#### FND MATCH BETWEEN LA and KM 
i<-1
j<-length(ecl_2018_raw_usage_desc$abc_classification_LA)
abc_match_LAvskm<-""

for (i in 1:j) {
  #print (i)
  
  if (ecl_2018_raw_usage_desc$abc_classification_LA[i] == ecl_2018_raw_usage_desc$abc_classification_km_ecl[i]) {
    abc_match_LAvskm[i] <- 1 
  }  else  {
    abc_match_LAvskm[i] <-0
  }
  
  
}
ecl_2018_raw_usage_desc <-cbind(ecl_2018_raw_usage_desc, abc_match_LAvskm) 

#### FND MATCH BETWEEN Legacy and KM 
i<-1
j<-length(ecl_2018_raw_usage_desc$abc_classification_LA)
abc_match_Legacyvskm<-""

for (i in 1:j) {
  #print (i)
  
  if (ecl_2018_raw_usage_desc$abc_classification_km_ecl[i] == ecl_2018_raw_usage_desc$abc_classification_legacy_ecl[i]) {
    abc_match_Legacyvskm[i] <- 1 
  }  else  {
    abc_match_Legacyvskm[i] <-0
  }
  
  
}
ecl_2018_raw_usage_desc <-cbind(ecl_2018_raw_usage_desc, abc_match_Legacyvskm) 

write.table(ecl_2018_raw_usage_desc, "ecl_2018_raw_usage_0725.csv", sep=",",row.names=FALSE, quote=FALSE)

library(easyGgplot2)
ecl_2018_raw_usage_desc$abc_classification_LA<-as.factor(ecl_2018_raw_usage_desc$abc_classification_LA)
ecl_2018_raw_usage_desc$abc_classification_legacy_ecl<-as.factor(ecl_2018_raw_usage_desc$abc_classification_legacy_ecl)
ecl_2018_raw_usage_desc$abc_classification_km_ecl<-as.factor(ecl_2018_raw_usage_desc$abc_classification_km_ecl)

x<-as.factor(na.omit(ecl_2018_raw_usage_desc$abc_classification_legacy_ecl))

library(plyr)
freq_LA<-count(ecl_2018_raw_usage_desc$abc_classification_LA)
freq_Legacy<- count(ecl_2018_raw_usage_desc$abc_classification_legacy_ecl)
freq_km<- count(ecl_2018_raw_usage_desc$abc_classification_km_ecl)

freq_LA$Category<-"Theory"
freq_LA <-freq_LA[c(3,1,2)]

freq_Legacy$Category<-"Ecolab"
freq_Legacy <-freq_Legacy[c(3,1,2)]

freq_km$Category<-"K-Means"
freq_km <-freq_km[c(3,1,2)]


freq_all<-rbind(freq_LA,freq_Legacy,freq_km  )

names(freq_all) <- c("Category", "Segment", "Inventory_count")
Percentage_Inventory<- round((freq_all$Inventory_count/1706)*100)
#freq_all<-freq_all[,-4]
freq_all <-cbind(freq_all, Percentage_Inventory)

ggplot2.barplot(data=freq_all, xName='Segment', yName="Percentage_Inventory",
                groupName='Category', position=position_dodge())
###########################################################################################################

## k-Means clustering on NLC Finished goods data
setwd("D:\\Nomadgene_2018\\Working_Papers_2018\\Ecolab-Inventory\\Data")
nlc_2018<- tbl_df(read.csv("NLC_QTY_CONSUMPTION_2018.csv",sep=",", header=TRUE, as.is=TRUE, fill=TRUE))
nlc_2018$Qty<-log(nlc_2018$Qty)
#nlc_2018$Standard_cost <-log(nlc_2018$Standard_cost)
##### Unscaled version
ggplot(nlc_2018, aes(Standard_cost, Qty, color = Plant)) + geom_point()
#####  Scaled Version 
nlc_2018_scaled <-scale(nlc_2018[2:3])
nlc_2018_scaled <-cbind(nlc_2018_scaled, nlc_2018[1])
nlc_2018_scaled <-cbind(nlc_2018_scaled, nlc_2018[4:7])
### reorder columns for easy read.. 
nlc_2018_scaled <-nlc_2018_scaled[c(3,1,2,4,5,6,7)]
ggplot(nlc_2018_scaled, aes(Standard_cost, Qty, color = Plant)) + geom_point()
###################Unscaled Clustering #########################################
set.seed(20)
cluster1 <- kmeans(nlc_2018[, c("Qty","Standard_cost")], 3, nstart = 20)
fviz_cluster(cluster1, nlc_2018[,2:3],palette = "Set2", geom = "point", ggtheme = theme_minimal())
#fviz_cluster(cluster1, nlc_2018[,2:3], geom = "point",ellipse.type = "norm")

###################Scaled Clustering #########################################
set.seed(20)
km.nlc.scaled <- kmeans(nlc_2018_scaled[, c("Qty","Standard_cost")], 3, nstart = 20)
fviz_cluster(km.nlc.scaled, nlc_2018_scaled[,2:3],palette = "Set2", geom = "point", ggtheme = theme_minimal())

nlc_cluster<-km.nlc.scaled$cluster
nlc_2018_scaled <- cbind(nlc_2018_scaled ,nlc_cluster)

###For now lets do it hard way AKA a For loop > create  a column vector called ABC_Classification and append that fucker to original data frame 
i<-1
j<-length(nlc_2018_scaled$nlc_cluster)
abc_classification_nlc<-""

for (i in 1:j) {
  #print (i)
  
  if (nlc_2018_scaled$nlc_cluster[i] == 1) {
    abc_classification_nlc[i] <- "A" } ###Low Cost-High consumption
  else if(nlc_2018_scaled$nlc_cluster[i] == 3) {
    abc_classification_nlc[i] <- "B" } ####Low Cost-lOW consumption
  else if(nlc_2018_scaled$nlc_cluster == 2) {
    abc_classification_nlc[i] <- "C"  } #####High Cost-Low consumption

}

nlc_2018_scaled <-cbind(nlc_2018_scaled, abc_classification_nlc) 

#fviz_cluster(km.scaled, nlc_2018_scaled[,2:3], geom = "point",ellipse.type = "norm")

ecl_gc_legacy <- tbl_df(read.csv("GC_Legacy_ELC.csv",sep=",", header=TRUE, as.is=TRUE, fill=TRUE))

### Our next step is to draw comparison between GC Legacy and Our ABC- Classification 
#### One idea would be to:
## Loop it... 
i<-1
j<-length(nlc_2018_scaled$nlc_cluster)
abc_classification_legacy_nlc<-""

for (i in 1:j) {
  #print (i)
  
  prod <-nlc_2018_scaled$Material[i]
  plant <-nlc_2018_scaled$Plant[i]
  
  tmp<- filter(ecl_gc_legacy,(Plant==plant & Products==prod ))
  if (length(tmp$Volume_ABC)==0) {
    
    abc_classification_legacy_nlc[i] <-"NA"
    print(abc_classification_legacy_nlc[i])
  }   else      {
    abc_classification_legacy_nlc[i] <-tmp$Volume_ABC
    print(abc_classification_legacy_nlc[i])
    
  }
  
  
}

nlc_2018_scaled <-cbind(nlc_2018_scaled, abc_classification_legacy_nlc) 

i<-1
j<-length(nlc_2018_scaled$nlc_cluster)
abc_match_minevslegacy_nlc<-""

for (i in 1:j) {
  #print (i)
  
  if (nlc_2018_scaled$abc_classification_nlc[i] == nlc_2018_scaled$abc_classification_legacy_nlc[i]) {
    abc_match_minevslegacy_nlc[i] <- 1 
  }  else  {
    abc_match_minevslegacy_nlc[i] <-0
  }
  
  
}
nlc_2018_scaled <-cbind(nlc_2018_scaled, abc_match_minevslegacy_nlc) 

match<-filter(nlc_2018_scaled,nlc_2018_scaled$abc_match_minevslegacy_nlc==1)


#### Lets save it for now... 
write.table(nlc_2018_scaled, "nlc_2018_scaled_07232018_1901.csv", sep=",",row.names=FALSE, quote=FALSE)


# # PAM clustering
# # ++++++++++++++++++++
require(cluster)
pam.res <- pam(nlc_2018[,2:3], 4)
#  # Visualize pam clustering
fviz_cluster(pam.res, geom = "point", ellipse.type = "norm")
# 
# # Hierarchical clustering
# # ++++++++++++++++++++++++
# # Use hcut() which compute hclust and cut the tree
 hc.cut <- hcut(nlc_2018[,2:3], k = 3, hc_method = "complete")
# # Visualize dendrogram
fviz_dend(hc.cut, show_labels = FALSE, rect = TRUE)
# # Visualize cluster
 fviz_cluster(hc.cut, ellipse.type = "convex")
# 
## ---------------------------------------------
 ## k-Means clustering on NLC Finished goods data for each plant 
 setwd("D:\\Nomadgene_2018\\Working_Papers_2018\\Ecolab-Inventory\\Data")
 
 nlc_2018<- tbl_df(read.csv("NLC_QTY_CONSUMPTION__Reordered_2018.csv",sep=",", header=TRUE, as.is=TRUE, fill=TRUE))
 ggplot(nlc_2018, aes(Standard_cost, log_Qty, color = Plant)) + geom_point()
 
 nlc_2018$Plant <-as.factor(nlc_2018$Plant)
 
 # "CN05" "CN07" "CN25" "CN28" "CN57" "CN58"
 
 cn05<- nlc_2018 %>%
   filter(Plant == "CN05") 
 
 set.seed(20)
 cn05.km <- kmeans(cn05[, c("log_Qty","Standard_cost")], 3, nstart = 20)
 cn05.km$cluster <- as.factor(cn05.km$cluster)
 fviz_cluster(cn05.km, cn05[,2:3], geom = "point",ellipse.type = "norm")
 
 
 
 cn07<- nlc_2018 %>%
   filter(Plant == "CN07") 
 
 set.seed(20)
 cn07.km <- kmeans(cn07[, c("log_Qty","Standard_cost")], 3, nstart = 20)
 cn07.km$cluster <- as.factor(cn07.km$cluster)
 fviz_cluster(cn07.km, cn07[,2:3], geom = "point",ellipse.type = "norm")

 
 
 cn25<- nlc_2018 %>%
   filter(Plant == "CN25") 
 
 set.seed(20)
 cn25.km <- kmeans(cn25[, c("log_Qty","Standard_cost")], 3, nstart = 20)
 cn25.km$cluster <- as.factor(cn25.km$cluster)
 fviz_cluster(cn25.km, cn25[,2:3], geom = "point",ellipse.type = "norm")
 
 cn28<- nlc_2018 %>%
   filter(Plant == "CN28") 
 
 set.seed(20)
 cn28.km <- kmeans(cn28[, c("log_Qty","Standard_cost")], 3, nstart = 20)
 cn28.km$cluster <- as.factor(cn28.km$cluster)
 fviz_cluster(cn28.km, cn28[,2:3], geom = "point",ellipse.type = "norm")
 
 
 cn57<- nlc_2018 %>%
   filter(Plant == "CN57") 
 
 set.seed(20)
 cn57.km <- kmeans(cn57[, c("log_Qty","Standard_cost")], 3, nstart = 20)
 cn57.km$cluster <- as.factor(cn57.km$cluster)
 fviz_cluster(cn57.km, cn57[,2:3], geom = "point",ellipse.type = "norm")

 ###################################################################################
 #### ECL_QTY_CONSUMPTION_Reordered_2018.csv
 ECL_2018<- tbl_df(read.csv("ECL_QTY_CONSUMPTION_Reordered_2018.csv",sep=",", header=TRUE, as.is=TRUE, fill=TRUE))
 set.seed(20)
 ecl.km <- kmeans( ECL_2018[, c("log_Qty","Standard_cost")], 3, nstart = 20)
 ecl.km$cluster <- as.factor(ecl.km$cluster)
 fviz_cluster(ecl.km, ECL_2018[,2:3], geom = "point",ellipse.type = "norm")
 
 
 ###################################################################################
 
 
 
 
 

