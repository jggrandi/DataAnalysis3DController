# Written by Jeronimo Grandi
# Septermber 2016

# Input: Dataframe of one task.
# Output: Plot of the users work distribution for each transformation;


# 0 Before start:
# 0.0 It uses dplyr,tidyr,ggplot2, grid. So, you have to install them. (ex: package.install("dplyr"))
# 0.1 Go to Session->Set Working Directory->Choose Directory. Select the folder Analysis->R
# 0.2 In the tab Environment, Click in Open. Select .RData
# 0.3 In the tab History, Click in Open. Select .Rhistory
# 1. The script works in a specific way for different quantity of users. So, define it beforehand in the variable "qntusers"
# 2. After qntusers defined, the in the line  "users <- select(..." of the if whith the qntusers defined, specify the data frame you want.
# 2.1 Pay atention to load the file with the same quantity of users as defined
# 2.2 The data frame has to be previously loaded in the Environment
# 3. Run the script.
# 3.1 I usually select the lines that I want to run and after run them with Ctrl+Enter



library(ggplot2)
library(grid)
library(dplyr)
library(tidyr)

qntusers <- 4
users <- select(X20.4.Task4.2016.06.10.16.49.45,ends_with(".1"),ends_with(".2"), ends_with(".3"),ends_with(".4"), -starts_with("User"),-starts_with("Con")) #select only user action in object transformation 

users[users>0.999999] <- 1 # remove the noise in the data 
rad2deg <- function(rad) {(rad * 180) / (pi)}

TaskTime <- transmute(X20.4.Task4.2016.06.10.16.49.45, TaskTime = Time - 420.2895)

U1Trans <- transmute(users, U1Trans = sqrt((Translation.X.1 * Translation.X.1) + (Translation.Y.1 * Translation.Y.1) + (Translation.Z.1 * Translation.Z.1))) #translation magnitude
U1Rot <- transmute(users, U1Rot = rad2deg(2*acos(Rotation.W.1))) #angle of rotation
U1Scale <- transmute(users, U1Scale = abs(Scalling.1-1)) #scale in absolute values
U1Cam <- transmute(users, U1Cam = rad2deg(2*acos(Camera.W.1)))#angle of cam rotation

if(qntusers == 2 |qntusers == 3 | qntusers == 4){
  U2Trans <- transmute(users, U2Trans = sqrt((Translation.X.2 * Translation.X.2) + (Translation.Y.2 * Translation.Y.2) + (Translation.Z.2 * Translation.Z.2)))
  U2Rot <- transmute(users, U2Rot = rad2deg(2*acos(Rotation.W.2)))  
  U2Scale <- transmute(users, U2Scale = abs(Scalling.2-1))
  U2Cam <- transmute(users, U2Cam = rad2deg(2*acos(Camera.W.2)))
}

if(qntusers == 3 | qntusers == 4){
  U3Trans <- transmute(users, U3Trans = sqrt((Translation.X.3 * Translation.X.3) + (Translation.Y.3 * Translation.Y.3) + (Translation.Z.3 * Translation.Z.3)))
  U3Rot <- transmute(users, U3Rot = rad2deg(2*acos(Rotation.W.3)))
  U3Scale <- transmute(users, U3Scale = abs(Scalling.3-1))
  U3Cam <- transmute(users, U3Cam = rad2deg(2*acos(Camera.W.3)))
}

if(qntusers == 4){
  U4Trans <- transmute(users, U4Trans = sqrt((Translation.X.4 * Translation.X.4) + (Translation.Y.4 * Translation.Y.4) + (Translation.Z.4 * Translation.Z.4)))
  U4Rot <- transmute(users, U4Rot = rad2deg(2*acos(Rotation.W.4)))
  U4Scale <- transmute(users, U4Scale = abs(Scalling.4-1))
  U4Cam <- transmute(users, U4Cam = rad2deg(2*acos(Camera.W.4)))
}


if(qntusers == 1 )
  tidyUsers <- data.frame(U1Trans,U1Rot,U1Scale,U1Cam) #put all together
if(qntusers == 2)
  tidyUsers <- data.frame(U1Trans,U1Rot,U1Scale,U1Cam,U2Trans,U2Rot,U2Scale,U2Cam) #put all together
if(qntusers == 3)
  tidyUsers <- data.frame(U1Trans,U1Rot,U1Scale,U1Cam,U2Trans,U2Rot,U2Scale,U2Cam,U3Trans,U3Rot,U3Scale,U3Cam) #put all together
if(qntusers == 4)
  tidyUsers <- data.frame(TaskTime,U1Trans,U1Rot,U1Scale,U1Cam,U2Trans,U2Rot,U2Scale,U2Cam,U3Trans,U3Rot,U3Scale,U3Cam,U4Trans,U4Rot,U4Scale,U4Cam) #put all together
tidyUsers <- head(tidyUsers,-100)


p1 <- ggplot() + 
  geom_line(data = tidyUsers, aes(x = TaskTime, y = U1Trans, color = "red")) +
  geom_line(data = tidyUsers, aes(x = TaskTime, y = U2Trans, color = "blue"))  +
  geom_line(data = tidyUsers, aes(x = TaskTime, y = U3Trans, color = "green"))  +
  geom_line(data = tidyUsers, aes(x = TaskTime, y = U4Trans, color = "black"))  +
  theme_bw()+
  scale_x_continuous(limits = c(0, 60), expand = c(0, 0),breaks = c(0,10,20,30,40,50,60))+
  scale_y_continuous(expand = c(0, 0))+
  xlab('data_date') +
  ylab('Translation')+
  theme(axis.title.x = element_blank(),axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.x=element_blank())

p2 <- ggplot() + 
  geom_line(data = tidyUsers, aes(x = TaskTime, y = U1Rot, color = "red")) +
  geom_line(data = tidyUsers, aes(x = TaskTime, y = U2Rot, color = "blue"))  +
  geom_line(data = tidyUsers, aes(x = TaskTime, y = U3Rot, color = "green"))  +
  geom_line(data = tidyUsers, aes(x = TaskTime, y = U4Rot, color = "black"))  +
  theme_bw()+
  scale_x_continuous(limits = c(0, 60), expand = c(0, 0),breaks = c(0,10,20,30,40,50,60))+
  scale_y_continuous(expand = c(0, 0))+
  xlab('data_date') +
  ylab('Rotation')+
  theme(axis.title.x = element_blank(),axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.x=element_blank())
p3 <- ggplot() + 
  geom_line(data = tidyUsers, aes(x = TaskTime, y = U1Scale, color = "red")) +
  geom_line(data = tidyUsers, aes(x = TaskTime, y = U2Scale, color = "blue"))  +
  geom_line(data = tidyUsers, aes(x = TaskTime, y = U3Scale, color = "green"))  +
  geom_line(data = tidyUsers, aes(x = TaskTime, y = U4Scale, color = "black"))  +
  theme_bw()+
  scale_x_continuous(limits = c(0, 60), expand = c(0, 0),breaks = c(0,10,20,30,40,50,60))+
  scale_y_continuous(expand = c(0, 0))+
  xlab('') +
  ylab('Scale')+
  theme(axis.title.x = element_blank(),axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.x=element_blank())
p4 <- ggplot() + 
  geom_line(data = tidyUsers, aes(x = TaskTime, y = U1Cam, color = "red")) +
  geom_line(data = tidyUsers, aes(x = TaskTime, y = U2Cam, color = "blue"))  +
  geom_line(data = tidyUsers, aes(x = TaskTime, y = U3Cam, color = "green"))  +
  geom_line(data = tidyUsers, aes(x = TaskTime, y = U4Cam, color = "black"))  +
  theme_bw()+
  scale_x_continuous(limits = c(0, 60), expand = c(0, 0),breaks = c(0,10,20,30,40,50,60))+
  scale_y_continuous(expand = c(0, 0))+
  xlab('Time (Seconds)') +
  ylab('Cam. Rotation')+

grid.newpage()
grid.draw(rbind(ggplotGrob(p1), ggplotGrob(p2), ggplotGrob(p3),ggplotGrob(p4),  size = "last"))



users <- select(X19.4.Task4.2016.06.10.13.55.26,ends_with(".1"),ends_with(".2"), ends_with(".3"),ends_with(".4"), -starts_with("User"),-starts_with("Con")) #select only user action in object transformation 

users[users>0.999999] <- 1 # remove the noise in the data 
rad2deg <- function(rad) {(rad * 180) / (pi)}

TaskTime <- transmute(X19.4.Task4.2016.06.10.13.55.26, TaskTime = Time - 272.8940)

U1Trans <- transmute(users, U1Trans = sqrt((Translation.X.1 * Translation.X.1) + (Translation.Y.1 * Translation.Y.1) + (Translation.Z.1 * Translation.Z.1))) #translation magnitude
U1Rot <- transmute(users, U1Rot = rad2deg(2*acos(Rotation.W.1))) #angle of rotation
U1Scale <- transmute(users, U1Scale = abs(Scalling.1-1)) #scale in absolute values
U1Cam <- transmute(users, U1Cam = rad2deg(2*acos(Camera.W.1)))#angle of cam rotation

if(qntusers == 2 |qntusers == 3 | qntusers == 4){
  U2Trans <- transmute(users, U2Trans = sqrt((Translation.X.2 * Translation.X.2) + (Translation.Y.2 * Translation.Y.2) + (Translation.Z.2 * Translation.Z.2)))
  U2Rot <- transmute(users, U2Rot = rad2deg(2*acos(Rotation.W.2)))  
  U2Scale <- transmute(users, U2Scale = abs(Scalling.2-1))
  U2Cam <- transmute(users, U2Cam = rad2deg(2*acos(Camera.W.2)))
}

if(qntusers == 3 | qntusers == 4){
  U3Trans <- transmute(users, U3Trans = sqrt((Translation.X.3 * Translation.X.3) + (Translation.Y.3 * Translation.Y.3) + (Translation.Z.3 * Translation.Z.3)))
  U3Rot <- transmute(users, U3Rot = rad2deg(2*acos(Rotation.W.3)))
  U3Scale <- transmute(users, U3Scale = abs(Scalling.3-1))
  U3Cam <- transmute(users, U3Cam = rad2deg(2*acos(Camera.W.3)))
}

if(qntusers == 4){
  U4Trans <- transmute(users, U4Trans = sqrt((Translation.X.4 * Translation.X.4) + (Translation.Y.4 * Translation.Y.4) + (Translation.Z.4 * Translation.Z.4)))
  U4Rot <- transmute(users, U4Rot = rad2deg(2*acos(Rotation.W.4)))
  U4Scale <- transmute(users, U4Scale = abs(Scalling.4-1))
  U4Cam <- transmute(users, U4Cam = rad2deg(2*acos(Camera.W.4)))
}


if(qntusers == 1 )
  tidyUsers <- data.frame(U1Trans,U1Rot,U1Scale,U1Cam) #put all together
if(qntusers == 2)
  tidyUsers <- data.frame(U1Trans,U1Rot,U1Scale,U1Cam,U2Trans,U2Rot,U2Scale,U2Cam) #put all together
if(qntusers == 3)
  tidyUsers <- data.frame(U1Trans,U1Rot,U1Scale,U1Cam,U2Trans,U2Rot,U2Scale,U2Cam,U3Trans,U3Rot,U3Scale,U3Cam) #put all together
if(qntusers == 4)
  tidyUsers <- data.frame(TaskTime,U1Trans,U1Rot,U1Scale,U1Cam,U2Trans,U2Rot,U2Scale,U2Cam,U3Trans,U3Rot,U3Scale,U3Cam,U4Trans,U4Rot,U4Scale,U4Cam) #put all together
tidyUsers <- head(tidyUsers,-1)
tidyUsers <- tidyUsers[-c(1, 2, 3), ]
p1 <- ggplot() + 
  geom_line(data = tidyUsers, aes(x = TaskTime, y = U1Trans, color = "red")) +
  geom_line(data = tidyUsers, aes(x = TaskTime, y = U2Trans, color = "blue"))  +
  geom_line(data = tidyUsers, aes(x = TaskTime, y = U3Trans, color = "green"))  +
  geom_line(data = tidyUsers, aes(x = TaskTime, y = U4Trans, color = "black"))  +
  theme_bw()+
  scale_x_continuous(limits = c(0, 60), expand = c(0, 0),breaks = c(0,10,20,30,40,50,60))+
  scale_y_continuous(limits = c(0, 0.4), expand = c(0, 0))+
  xlab('data_date') +
  ylab('Translation')+
  theme(axis.title.x = element_blank(),axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.x=element_blank())

p2 <- ggplot() + 
  geom_line(data = tidyUsers, aes(x = TaskTime, y = U1Rot, color = "red")) +
  geom_line(data = tidyUsers, aes(x = TaskTime, y = U2Rot, color = "blue"))  +
  geom_line(data = tidyUsers, aes(x = TaskTime, y = U3Rot, color = "green"))  +
  geom_line(data = tidyUsers, aes(x = TaskTime, y = U4Rot, color = "black"))  +
  theme_bw()+
  scale_x_continuous(limits = c(0, 60), expand = c(0, 0),breaks = c(0,10,20,30,40,50,60))+
  scale_y_continuous(limits = c(0, 10), expand = c(0, 0))+
  xlab('data_date') +
  ylab('Rotation')+
  theme(axis.title.x = element_blank(),axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.x=element_blank())
p3 <- ggplot() + 
  geom_line(data = tidyUsers, aes(x = TaskTime, y = U1Scale, color = "red")) +
  geom_line(data = tidyUsers, aes(x = TaskTime, y = U2Scale, color = "blue"))  +
  geom_line(data = tidyUsers, aes(x = TaskTime, y = U3Scale, color = "green"))  +
  geom_line(data = tidyUsers, aes(x = TaskTime, y = U4Scale, color = "black"))  +
  theme_bw()+
  scale_x_continuous(limits = c(0, 60), expand = c(0, 0),breaks = c(0,10,20,30,40,50,60))+
  scale_y_continuous(limits = c(0, 0.04), expand = c(0, 0),breaks = c(0,0.01,0.02,0.03,0.04))+
  xlab('') +
  ylab('Scale')+
  theme(axis.title.x = element_blank(),axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.x=element_blank())
p4 <- ggplot() + 
  geom_line(data = tidyUsers, aes(x = TaskTime, y = U1Cam, color = "red")) +
  geom_line(data = tidyUsers, aes(x = TaskTime, y = U2Cam, color = "blue"))  +
  geom_line(data = tidyUsers, aes(x = TaskTime, y = U3Cam, color = "green"))  +
  geom_line(data = tidyUsers, aes(x = TaskTime, y = U4Cam, color = "black"))  +
  theme_bw()+
  scale_x_continuous(limits = c(0, 60), expand = c(0, 0),breaks = c(0,10,20,30,40,50,60))+
  scale_y_continuous(expand = c(0, 0))+
  xlab('Time (Seconds)') +
  ylab('Cam. Rotation')+
  
grid.newpage()
grid.draw(rbind(ggplotGrob(p1), ggplotGrob(p2), ggplotGrob(p3),ggplotGrob(p4),  size = "last"))




#' Create some data to play with. Two time series with the same timestamp.
#df <- data.frame(DateTime = c(0:8760) * 2, series1 = rnorm(8761), series2 = rnorm(8761, 100))

#' Create the two plots.
#plot1 <- df %>%
#  select(DateTime, series1) %>%
#  na.omit() %>%
#  ggplot() +
#  geom_point(aes(x = DateTime, y = series1), size = 0.5, alpha = 0.75) +
#  ylab("Red dots / m") +
#  theme_minimal() +
#  theme(axis.title.x = element_blank())

#plot2 <- df %>%
#  select(DateTime, series2) %>%
#  na.omit() %>%
#  ggplot() +
#  geom_point(aes(x = DateTime, y = series2), size = 0.5, alpha = 0.75) +
#  ylab("Blue drops / L") +
#  theme_minimal() +
#  theme(axis.title.x = element_blank())

#grid.newpage()
#grid.draw(rbind(ggplotGrob(plot1), ggplotGrob(plot2), size = "last"))
