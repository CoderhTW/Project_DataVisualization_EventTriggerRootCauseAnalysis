#import Appier assessment data
event_A_logs <- read.csv("~/Desktop/Appier Data/event_A_logs.csv")
event_B_logs <- read.csv("~/Desktop/Appier Data/event_B_logs.csv")

#install/load sqldf and ggplot2 package
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}
packages <- c("ggplot2", "sqldf","anytime")
ipak(packages)

#transform event time into time data format
event_A_logs$event_time<-anytime(event_A_logs$event_time)
event_B_logs$event_time<-anytime(event_B_logs$event_time)

#primary key and observation count check
summary(event_A_logs)
summary(event_B_logs)

#label event B
event_B_logs$triggered<-rep("Y",dim(event_B_logs)[1])

#merge two files by tracking_id as the primary key
logs<-sqldf("
            SELECT A.*, B.event_time as event_time_B, B.triggered
            FROM event_A_logs as A
            LEFT JOIN event_B_logs as B
            ON A.tracking_id=B.tracking_id;
            ")
attach(logs)

#replace NA event B time by maximum of event time A
right_sensor<-max(event_time)
logs$event_time_B<-anytime(replace(logs$event_time_B,is.na(logs$event_time_B),right_sensor))

#replace NA triggered label with "N"
logs$triggered<-replace(logs$triggered,is.na(logs$triggered),"N")

#seperate logs by if event B is triggered or not
logs_norm<-logs[logs$triggered=="N",]
logs_trig<-logs[logs$triggered=="Y",]

#View data summary to find material cluster(s)
des_stat<-cbind(summary(logs_trig$cluster),summary(logs$cluster),summary(logs_trig$cluster)/summary(logs$cluster))[1:14,]
colnames(des_stat)<-c("event B count","event A count","event B trigger rate")
View(des_stat) # 66 records of event B are not triggered by event A

#visualize event B trigger time (all data)
logs_trig$interval<-as.numeric(logs_trig$event_time_B-logs_trig$event_time)
ggplot(data=logs_trig)+
  geom_histogram(aes(x=interval))+
  xlab("all cluster")

#calculate 90%, 95%, and 99% percentile of event B trigger time
quantile(logs_trig$interval, c(.90, .95, .99))

#visualize event B trigger time by cluster 
logs_trig$interval<-logs_trig$event_time_B-logs_trig$event_time
for (i in 1:length(unique(logs_trig$cluster))){
  data_cluster<-logs_trig[logs_trig$cluster==unique(logs_trig$cluster)[i],]
  p1<-eval(substitute(
    ggplot(data=data_cluster)+
      geom_histogram(aes(x=interval))+
      xlab(unique(logs_trig$cluster)[i])+
      xlim(-10000,200000)+
      ylim(0,1500)
    ,list(i = i)))
  print(i)
  print(p1)
  myplots[[i]] <- p1  # add each plot into plot list
}

#visualize patterns of event A and event B by cluster (x=event_time)
for (i in 1:length(unique(logs$cluster))){
  data_cluster<-logs[logs$cluster==unique(logs$cluster)[i],]
  p1<-eval(substitute(
    ggplot(data=data_cluster,aes(event_time,fill=triggered))+
      geom_density(alpha=0.2)+
      xlab(unique(logs$cluster)[i])
    ,list(i = i)))
  print(i)
  print(p1)
  myplots[[i]] <- p1  # add each plot into plot list
}

#anomly investigate (cluster:e581850f705724caebf21f35fdc1395da88a9388)
anomly_e5818<-logs_trig[logs_trig$cluster=="e581850f705724caebf21f35fdc1395da88a9388",]
head(summary(anomly_e5818$machine_id))
ggplot(data=anomly_e5818[anomly_e5818$machine_id=="1645cbc3cfb3dc782fcd0816850c789196133fe1",])+geom_histogram(aes(x=event_time))

#anomaly investigate (cluster:b3a012651bad958d7527618fc6a044654192b498)
anomly_b3a01<-logs_trig[logs_trig$cluster=="b3a012651bad958d7527618fc6a044654192b498",]
head(summary(anomly_b3a01$machine_id))
ggplot(data=anomly_b3a01[anomly_b3a01$machine_id=="9a5d2ac40cfe19e56f625bb803fd7086882e660b",])+geom_histogram(aes(x=event_time))

#anomaly investigate (cluster:6e65ccd2dcb418d0bdb844179b2418294e2292a0)
anomly_6e65c<-logs_trig[logs_trig$cluster=="6e65ccd2dcb418d0bdb844179b2418294e2292a0",]
head(summary(anomly_6e65c$machine_id))
ggplot(data=anomly_6e65c[anomly_6e65c$machine_id=="cb1c56ec09f95d8df5496de8776dc26b07eb7ad9",])+geom_histogram(aes(x=event_time))

#anomaly investigate (cluster:bafb42b2de21d125a905d935f9b6b77b7323df75)
anomly_babf4<-logs_trig[logs_trig$cluster=="bafb42b2de21d125a905d935f9b6b77b7323df75",]
head(summary(anomly_babf4$machine_id))
ggplot(data=anomly_babf4[anomly_babf4$machine_id=="00d4ceae26b5905db33bca5a653c493b6df6ad01",])+geom_histogram(aes(x=event_time))
lims <- as.POSIXct(strptime(c("2017-05-07 00:00:00","2017-05-07 23:59:59"), format = "%Y-%m-%d %H:%M")) 
ggplot(data=anomly_babf4[anomly_babf4$machine_id=="9e9ee26d5678cc7dda90c480e727f0e1032dc9df",])+geom_histogram(aes(x=event_time))+scale_x_datetime(limits =lims)


