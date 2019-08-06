rm(list=ls());
library(changepoint)
library(mice)
library(VIM)
library(sqldf)
setwd("C:\\bhupi\\Personal\\2016\\Anoop\\climate19502010");
d<-read.table("climate19502010.csv", sep=",", header=TRUE, fill=TRUE, as.is=TRUE)
# lets try a simple trick by getting rid of all rows with NAs.
d<-d[!rowSums(is.na(d[1:5])), ]

sqldf(); 
d_lon_lat <-sqldf("select distinct d.lon,d.lat from d");
d_lon_lat_count <-sqldf("select count(*) as count from d_lon_lat");

#lets create a Temp and Prec matrices...will make it much easier to perform other operations. 
#first we create a look-up table for lat-lon
d_lon_lat_lookup<-cbind(ID=1:d_lon_lat_count$count,Longitude = d_lon_lat$Lon,Latitude=d_lon_lat$Lat)

## essentally we create a matrix 60 rows by 3843 columns. 
i=0; 
ncpts_temp_prec_ALL<-NULL
count<-d_lon_lat_count$count
for(i in 1:count) {
  #this statement errors out.... 
  a_lon<-d_lon_lat$Lon[i]
  a_lat<-d_lon_lat$Lat[i]
  d_subset <-d[d$Lon==a_lon & d$Lat==a_lat,]
  # now we need to make monthly slices from this subsetted data.
  d_mnth_lookup<-cbind(Mnth_ID=1:12,d_subset) #this is bad bad way of doing indexing. this should have been derived from TimeP 
  
    j=0; 
    month<-0
    for (j in 1:12){
      d_month = d_mnth_lookup[d_mnth_lookup$Mnth_ID==j,]
      #lets build changepoints for temprature and pressure /month /lat-lon combination 
      m.temp.pelt.pen <- cpt.mean(d_month$Temp, penalty = "Manual", pen.value = "1.5 * log(n)",method = "PELT")
      ncpt_temp<-ncpts(m.temp.pelt.pen)
      m.prec.pelt.pen <- cpt.mean(d_month$Prec, penalty = "Manual", pen.value = "1.5 * log(n)",method = "PELT")
      ncpt_prec<-ncpts(m.prec.pelt.pen)
      month<-j
      
      
      #each lat/lon combination would generate 12 records for #ncpts.  
      #we would store <lat , lon , month,  ncpts>  temp + prec as <a_lat,a_lon,j,ncpt_temp, ncpt_prec>
      new_rows<-cbind(a_lon,a_lat,month,ncpt_temp, ncpt_prec,deparse.level = 2)
      ncpts_temp_prec_ALL<-rbind(ncpts_temp_prec_ALL,new_rows)
      
      
    }
  
  
  
  
}

file_like_list<-paste("C:\\bhupi\\Personal\\2016\\Anoop\\climate19502010\\NCPTS_LATEST_03_15_2016.CSV",sep="")
write.csv(ncpts_temp_prec_ALL,file_like_list)

##################################################################################################

