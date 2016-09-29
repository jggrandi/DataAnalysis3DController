# Maybe it is the ugliest script written in R ever. But, it works. How? I'll explain below. (You are lucky!)
# First, let me introduce myself. I'm Jeronimo and I'll be your guide. 

# Input: Dataframe of one task.
# Output: Load by task total time of each user;
#         Load by worked time of each user;
#         Load per transformation of each user;

# 0 Before start:
# 0.0 It uses dplyr and tidyr. So, you have to install them. (ex: package.install("dplyr"))
# 0.1 Go to Session->Set Working Directory->Choose Directory. Select the folder Analysis->R
# 0.2 In the tab Environment, Click in Open. Select .RData
# 0.3 In the tab History, Click in Open. Select .Rhistory
# 1. If it's the first time that you are running this script: 
# 1.1 Search for all comments "if first" and uncomment them;
# 1.2 Serach for all comments "else add to the exiting" and comment them;
# 1.3 You have to change "_taskX" (X is the number of the task) with the number of the task of the file loaded 
# 2. After the first run, do the opposite. Comment "if first" and uncomment "else..."
# 3. The script works in a specific way for different quantity of users. So, define it beforehand in the variable "qntusers"
# 4. After qntusers defined, the in the line  "users <- select(..." of the if whith the qntusers defined, specify the data frame you want.
# 4.1 Pay atention to load the file with the same quantity of users as defined
# 4.2 The data frame has to be previously loaded in the Environment
# 5. Run the script.
# 5.1 I usually select the lines that I want to run and after run them with Ctrl+Enter  

#tidy3Task3 <- subset(`3-Task3`, select=c(Time,Translation.X.1,Translation.Y.1,Translation.Z.1,Rotation.X.1,Rotation.Y.1,Rotation.Z.1,Rotation.W.1,Scalling.1,Translation.X.2,Translation.Y.2,Translation.Z.2,Rotation.X.2,Rotation.Y.2,Rotation.Z.2,Rotation.W.2,Scalling.2,Translation.X.3,Translation.Y.3,Translation.Z.3,Rotation.X.3,Rotation.Y.3,Rotation.Z.3,Rotation.W.3,Scalling.3,Translation.X.4,Translation.Y.4,Translation.Z.4,Rotation.X.4,Rotation.Y.4,Rotation.Z.4,Rotation.W.4,Scalling.4))
#tidyUsers <- tidyUsers[-c(3253),] ## to remove a specific row
#U1Trans <- head(U1Trans,-1) ## to remove the last row 
#nrow(tidyUsers)
#bind_rows(U4Cam,U4Trans)

library(dplyr)
library(tidyr)

qntusers <- 4
if(qntusers == 1)
  users <- select(X23.1.Task1.2016.06.10.18.32.14,ends_with(".1"), -starts_with("User"),-starts_with("Con")) #select only user action in object transformation 
if(qntusers == 2)
  users <- select(X6.2.Task3.2016.06.07.10.18.40,ends_with(".1"),ends_with(".2"), -starts_with("User"),-starts_with("Con")) #select only user action in object transformation 
if(qntusers == 3)
  users <- select(X18.3.Task1.2016.06.09.10.19.23,ends_with(".1"),ends_with(".2"), ends_with(".3"), -starts_with("User"),-starts_with("Con")) #select only user action in object transformation 
if(qntusers == 4)
  users <- select(X25.4.Task4.2016.06.17.14.31.44,ends_with(".1"),ends_with(".2"), ends_with(".3"),ends_with(".4"), -starts_with("User"),-starts_with("Con")) #select only user action in object transformation 

users[users>0.999999] <- 1 # remove the noise in the data 
rad2deg <- function(rad) {(rad * 180) / (pi)}


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
  tidyUsers <- data.frame(U1Trans,U1Rot,U1Scale,U1Cam,U2Trans,U2Rot,U2Scale,U2Cam,U3Trans,U3Rot,U3Scale,U3Cam,U4Trans,U4Rot,U4Scale,U4Cam) #put all together
tidyUsers <- head(tidyUsers,-1)

if(qntusers == 1){
  loadTrans <- transmute(tidyUsers, loadTrans = U1Trans) #total translation actions performed by all users
  loadScale <- transmute(tidyUsers, loadScale = U1Scale) #total scale actions performed by all users
  loadRot <- transmute(tidyUsers, loadRot = U1Rot) #total rotation actions performed by all users
  loadCam <- transmute(tidyUsers, loadCam = U1Cam) #total camera actions performed by all users;
}

if(qntusers == 2){
  loadTrans <- transmute(tidyUsers, loadTrans = U1Trans + U2Trans) #total translation actions performed by all users
  loadScale <- transmute(tidyUsers, loadScale = U1Scale + U2Scale) #total scale actions performed by all users
  loadRot <- transmute(tidyUsers, loadRot = U1Rot + U2Rot) #total rotation actions performed by all users
  loadCam <- transmute(tidyUsers, loadCam = U1Cam + U2Cam) #total camera actions performed by all users;
}

if(qntusers == 3){
  loadTrans <- transmute(tidyUsers, loadTrans = U1Trans + U2Trans + U3Trans) #total translation actions performed by all users
  loadScale <- transmute(tidyUsers, loadScale = U1Scale + U2Scale + U3Scale) #total scale actions performed by all users
  loadRot <- transmute(tidyUsers, loadRot = U1Rot + U2Rot + U3Rot) #total rotation actions performed by all users
  loadCam <- transmute(tidyUsers, loadCam = U1Cam + U2Cam + U3Cam) #total camera actions performed by all users;
}

if(qntusers == 4){
  loadTrans <- transmute(tidyUsers, loadTrans = U1Trans + U2Trans + U3Trans + U4Trans) #total translation actions performed by all users
  loadScale <- transmute(tidyUsers, loadScale = U1Scale + U2Scale + U3Scale + U4Scale) #total scale actions performed by all users
  loadRot <- transmute(tidyUsers, loadRot = U1Rot + U2Rot + U3Rot + U4Rot) #total rotation actions performed by all users
  loadCam <- transmute(tidyUsers, loadCam = U1Cam + U2Cam + U3Cam + U4Cam) #total camera actions performed by all users;
}

loadTotal <- loadTrans + loadRot + loadScale + loadCam

loadTrans <- head(loadTrans,-1) # remove the last row 
loadScale <- head(loadScale,-1)
loadRot <- head(loadRot,-1)
loadCam <- head(loadCam,-1)
loadTotal <- head(loadTotal,-1)


if(qntusers == 1 | qntusers == 2 |qntusers == 3 | qntusers == 4){
  U1Trans <- head(U1Trans,-1) # remove the last row
  U1Rot <- head(U1Rot,-1)
  U1Scale <- head(U1Scale,-1) # remove the last row 
  U1Cam <- head(U1Cam,-1) # remove the last row 
  U1totalTrans <- colSums(U1Trans != 0) / colSums(loadTrans != 0)
  U1totalRot <- colSums(U1Rot != 0) / colSums(loadRot != 0)
  U1totalScale <- colSums(U1Scale != 0) / colSums(loadScale != 0)
  U1totalCam <- colSums(U1Cam != 0) / colSums(loadCam != 0)
  allUsersTotalTrans <- U1totalTrans  #value >1 means parallel actions 
  allUsersTotalRot <- U1totalRot  #value >1 means parallel actions 
  allUsersTotalScale <- U1totalScale  #value >1 means parallel actions 
  allUsersTotalCam <- U1totalCam  #value >1 means parallel actions 
}


if(qntusers == 2 |qntusers == 3 | qntusers == 4){
  U2Trans <- head(U2Trans,-1)
  U2Rot <- head(U2Rot,-1)
  U2Scale <- head(U2Scale,-1)
  U2Cam <- head(U2Cam,-1)
  U2totalTrans <- colSums(U2Trans != 0) / colSums(loadTrans != 0)
  U2totalRot <- colSums(U2Rot != 0) / colSums(loadRot != 0)
  U2totalScale <- colSums(U2Scale != 0) / colSums(loadScale != 0)
  U2totalCam <- colSums(U2Cam != 0) / colSums(loadCam != 0)
  allUsersTotalTrans <- U1totalTrans + U2totalTrans #value >1 means parallel actions 
  allUsersTotalRot <- U1totalRot + U2totalRot #value >1 means parallel actions 
  allUsersTotalScale <- U1totalScale + U2totalScale #value >1 means parallel actions 
  allUsersTotalCam <- U1totalCam + U2totalCam #value >1 means parallel actions 
  
}

if(qntusers == 3 | qntusers == 4){
  U3Trans <- head(U3Trans,-1)
  U3Rot <- head(U3Rot,-1)
  U3Scale <- head(U3Scale,-1)
  U3Cam <- head(U3Cam,-1)
  U3totalTrans <- colSums(U3Trans != 0) / colSums(loadTrans != 0)
  U3totalRot <- colSums(U3Rot != 0) / colSums(loadRot != 0)
  U3totalScale <- colSums(U3Scale != 0) / colSums(loadScale != 0)
  U3totalCam <- colSums(U3Cam != 0) / colSums(loadCam != 0)
  allUsersTotalTrans <- U1totalTrans + U2totalTrans +U3totalTrans#value >1 means parallel actions 
  allUsersTotalRot <- U1totalRot + U2totalRot +U3totalRot#value >1 means parallel actions 
  allUsersTotalScale <- U1totalScale + U2totalScale +U3totalScale#value >1 means parallel actions 
  allUsersTotalCam <- U1totalCam + U2totalCam +U3totalCam #value >1 means parallel actions 
  
}

if(qntusers == 4){
  U4Trans <- head(U4Trans,-1)
  U4Rot <- head(U4Rot,-1)
  U4Scale <- head(U4Scale,-1)
  U4Cam <- head(U4Cam,-1)
  U4totalTrans <- colSums(U4Trans != 0) / colSums(loadTrans != 0)
  U4totalRot <- colSums(U4Rot != 0) / colSums(loadRot != 0)
  U4totalScale <- colSums(U4Scale != 0) / colSums(loadScale != 0)
  U4totalCam <- colSums(U4Cam != 0) / colSums(loadCam != 0)
  allUsersTotalTrans <- U1totalTrans + U2totalTrans +U3totalTrans +U4totalTrans #value >1 means parallel actions 
  allUsersTotalRot <- U1totalRot + U2totalRot +U3totalRot +U4totalRot #value >1 means parallel actions 
  allUsersTotalScale <- U1totalScale + U2totalScale +U3totalScale +U4totalScale #value >1 means parallel actions 
  allUsersTotalCam <- U1totalCam + U2totalCam +U3totalCam +U4totalCam #value >1 means parallel actions 
}

taskTotalLoad <- colSums(loadTotal != 0) / nrow(loadTotal) # task total load
taskTotalLoad <-as.data.frame(taskTotalLoad)


timeWorked <- colSums(loadTotal != 0) # get only the time worked

loadByTotalTime <- colSums(tidyUsers != 0) / nrow(tidyUsers) # load of each action of each users by total time
loadByWorkedTime <- colSums(tidyUsers != 0) / timeWorked # load of each action of each users only in the worked period

loadByWorkedTime <- as.data.frame.array(t(loadByWorkedTime))
loadByTotalTime <- as.data.frame.array(t(loadByTotalTime))
loadByTotalTime <- bind_cols(loadByTotalTime,taskTotalLoad)


if(qntusers == 1){
  loadPerUser <- data.frame(U1totalTrans,U1totalRot,U1totalScale,U1totalCam,allUsersTotalTrans,allUsersTotalRot,allUsersTotalScale,allUsersTotalCam)
  #loadPerUser1_task3 <- loadPerUser #if first
  #loadByTotalTime1_task3 <- loadByTotalTime #if first
  #loadByWorkedTime1_task3 <- loadByWorkedTime #if first
  loadPerUser1_task3 <- bind_rows(loadPerUser1_task3,loadPerUser) # else add to the exiting
  loadByTotalTime1_task3<- bind_rows(loadByTotalTime1_task3,loadByTotalTime) # else add to the exiting
  loadByWorkedTime1_task3<- bind_rows(loadByWorkedTime1_task3,loadByWorkedTime) # else add to the exiting

}
if(qntusers == 2){
  loadPerUser <- data.frame(U1totalTrans,U1totalRot,U1totalScale,U1totalCam,U2totalTrans,U2totalRot,U2totalScale,U2totalCam,allUsersTotalTrans,allUsersTotalRot,allUsersTotalScale,allUsersTotalCam)
  #loadPerUser2_task3 <- loadPerUser #if first
  #loadByTotalTime2_task3 <- loadByTotalTime #if first
  #loadByWorkedTime2_task3 <- loadByWorkedTime #if first
  loadPerUser2_task3 <- bind_rows(loadPerUser2_task3,loadPerUser) # else add to the exiting
  loadByTotalTime2_task3 <- bind_rows(loadByTotalTime2_task3,loadByTotalTime) # else add to the exiting
  loadByWorkedTime2_task3 <- bind_rows(loadByWorkedTime2_task3,loadByWorkedTime) # else add to the exiting
}
if(qntusers == 3){
  loadPerUser <- data.frame(U1totalTrans,U1totalRot,U1totalScale,U1totalCam,U2totalTrans,U2totalRot,U2totalScale,U2totalCam,U3totalTrans,U3totalRot,U3totalScale,U3totalCam,allUsersTotalTrans,allUsersTotalRot,allUsersTotalScale,allUsersTotalCam)
  #loadPerUser3_task3 <- loadPerUser #if first
  #loadByTotalTime3_task3 <- loadByTotalTime #if first
  #loadByWorkedTime3_task3 <- loadByWorkedTime #if first
  loadPerUser3_task3 <- bind_rows(loadPerUser3_task3,loadPerUser) # else add to the exiting
  loadByTotalTime3_task3 <- bind_rows(loadByTotalTime3_task3,loadByTotalTime) # else add to the exiting
  loadByWorkedTime3_task3 <- bind_rows(loadByWorkedTime3_task3,loadByWorkedTime) # else add to the exiting

}

if(qntusers == 4){
  loadPerUser <- data.frame(U1totalTrans,U1totalRot,U1totalScale,U1totalCam,U2totalTrans,U2totalRot,U2totalScale,U2totalCam,U3totalTrans,U3totalRot,U3totalScale,U3totalCam,U4totalTrans,U4totalRot,U4totalScale,U4totalCam,allUsersTotalTrans,allUsersTotalRot,allUsersTotalScale,allUsersTotalCam)
  #loadPerUser4_task3 <- loadPerUser #if first
  #loadByTotalTime4_task3 <- loadByTotalTime #if first
  #loadByWorkedTime4_task3 <- loadByWorkedTime #if first
  loadPerUser4_task4 <- bind_rows(loadPerUser4_task4,loadPerUser) # else add to the exiting
  loadByTotalTime4_task4 <- bind_rows(loadByTotalTime4_task4,loadByTotalTime) # else add to the exiting
  loadByWorkedTime4_task4 <- bind_rows(loadByWorkedTime4_task4,loadByWorkedTime) # else add to the exiting
}


rm(U1Trans,U2Trans,U3Trans,U4Trans)
rm(U1Rot,U2Rot,U3Rot,U4Rot)
rm(U1Scale,U2Scale,U3Scale,U4Scale)
rm(U1Cam,U2Cam,U3Cam,U4Cam)

rm(U1totalTrans,U2totalTrans,U3totalTrans,U4totalTrans)
rm(U1totalRot,U2totalRot,U3totalRot,U4totalRot)
rm(U1totalScale,U2totalScale,U3totalScale,U4totalScale)
rm(U1totalCam,U2totalCam,U3totalCam,U4totalCam)

rm(loadTrans,loadRot,loadScale,loadCam,loadTotal)

rm(allUsersTotalTrans,allUsersTotalRot,allUsersTotalScale,allUsersTotalCam)
rm(timeWorked,taskTotalLoad, loadByWorkedTime,loadByTotalTime)
rm(tidyUsers,users)
rm(loadPerUser)


#write.csv(loadByTotalTime1, file = "loadByTotalTime1_task3")
#write.csv(loadByTotalTime2, file = "loadByTotalTime2_task3")
#write.csv(loadByTotalTime3, file = "loadByTotalTime3_task3")
#write.csv(loadByTotalTime4, file = "loadByTotalTime4_task3")
#write.csv(loadByWorkedTime1, file = "loadByWorkedTime1_task3.csv")
#write.csv(loadByWorkedTime2, file = "loadByWorkedTime2_task3.csv")
#write.csv(loadByWorkedTime3, file = "loadByWorkedTime3_task3.csv")
#write.csv(loadByWorkedTime4, file = "loadByWorkedTime4_task3.csv")
#write.csv(loadPerUser1, file = "loadPerUser1_task3.csv")
#write.csv(loadPerUser2, file = "loadPerUser2_task3.csv")
#write.csv(loadPerUser3, file = "loadPerUser3_task3.csv")
#write.csv(loadPerUser4, file = "loadPerUser4_task3.csv")


