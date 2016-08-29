#tidy3Task3 <- subset(`3-Task3`, select=c(Time,Translation.X.1,Translation.Y.1,Translation.Z.1,Rotation.X.1,Rotation.Y.1,Rotation.Z.1,Rotation.W.1,Scalling.1,Translation.X.2,Translation.Y.2,Translation.Z.2,Rotation.X.2,Rotation.Y.2,Rotation.Z.2,Rotation.W.2,Scalling.2,Translation.X.3,Translation.Y.3,Translation.Z.3,Rotation.X.3,Rotation.Y.3,Rotation.Z.3,Rotation.W.3,Scalling.3,Translation.X.4,Translation.Y.4,Translation.Z.4,Rotation.X.4,Rotation.Y.4,Rotation.Z.4,Rotation.W.4,Scalling.4))
#tidyUsers <- tidyUsers[-c(3253),] ## to remove a specific row
#U1Trans <- head(U1Trans,-1) ## to remove the last row 
#nrow(tidyUsers)
#bind_rows(U4Cam,U4Trans)

library(dplyr)
library(tidyr)

users <- select(task,ends_with(".1"),ends_with(".2"), ends_with(".3"),ends_with(".4"), -starts_with("User"),-starts_with("Con")) #select only user action in object transformation 
users[users>0.999999] <- 1 # remove the noise in the data 

U1Trans <- transmute(users, U1Trans = sqrt((Translation.X.1 * Translation.X.1) + (Translation.Y.1 * Translation.Y.1) + (Translation.Z.1 * Translation.Z.1))) #translation magnitude
U2Trans <- transmute(users, U2Trans = sqrt((Translation.X.2 * Translation.X.2) + (Translation.Y.2 * Translation.Y.2) + (Translation.Z.2 * Translation.Z.2)))
U3Trans <- transmute(users, U3Trans = sqrt((Translation.X.3 * Translation.X.3) + (Translation.Y.3 * Translation.Y.3) + (Translation.Z.3 * Translation.Z.3)))
U4Trans <- transmute(users, U4Trans = sqrt((Translation.X.4 * Translation.X.4) + (Translation.Y.4 * Translation.Y.4) + (Translation.Z.4 * Translation.Z.4)))

rad2deg <- function(rad) {(rad * 180) / (pi)}

U1Rot <- transmute(users, U1Rot = rad2deg(2*acos(Rotation.W.1))) #angle of rotation
U2Rot <- transmute(users, U2Rot = rad2deg(2*acos(Rotation.W.2)))
U3Rot <- transmute(users, U3Rot = rad2deg(2*acos(Rotation.W.3)))
U4Rot <- transmute(users, U4Rot = rad2deg(2*acos(Rotation.W.4)))

U1Scale <- transmute(users, U1Scale = abs(Scalling.1-1)) #scale in absolute values
U2Scale <- transmute(users, U2Scale = abs(Scalling.2-1))
U3Scale <- transmute(users, U3Scale = abs(Scalling.3-1))
U4Scale <- transmute(users, U4Scale = abs(Scalling.4-1))

U1Cam <- transmute(users, U1Cam = rad2deg(2*acos(Camera.W.1)))#angle of cam rotation
U2Cam <- transmute(users, U2Cam = rad2deg(2*acos(Camera.W.2)))
U3Cam <- transmute(users, U3Cam = rad2deg(2*acos(Camera.W.3)))
U4Cam <- transmute(users, U4Cam = rad2deg(2*acos(Camera.W.4)))

tidyUsers <- data.frame(U1Trans,U1Rot,U1Scale,U1Cam,U2Trans,U2Rot,U2Scale,U2Cam,U3Trans,U3Rot,U3Scale,U3Cam,U4Trans,U4Rot,U4Scale,U4Cam) #put all together
tidyUsers <- head(tidyUsers,-1)

loadTrans <- transmute(tidyUsers, loadTrans = U1Trans + U2Trans + U3Trans + U4Trans) #total translation actions performed by all users
loadScale <- transmute(tidyUsers, loadScale = U1Scale + U2Scale + U3Scale + U4Scale) #total scale actions performed by all users
loadRot <- transmute(tidyUsers, loadRot = U1Rot + U2Rot + U3Rot + U4Rot) #total rotation actions performed by all users
loadCam <- transmute(tidyUsers, loadCam = U1Cam + U2Cam + U3Cam + U4Cam) #total camera actions performed by all users;
loadTotal <- loadTrans + loadRot + loadScale + loadCam

loadTrans <- head(loadTrans,-1) # remove the last row 
loadScale <- head(loadScale,-1)
loadRot <- head(loadRot,-1)
loadCam <- head(loadCam,-1)
loadTotal <- head(loadTotal,-1)

U1Trans <- head(U1Trans,-1) # remove the last row 
U2Trans <- head(U2Trans,-1)
U3Trans <- head(U3Trans,-1)
U4Trans <- head(U4Trans,-1)

U1totalTrans <- colSums(U1Trans != 0) / colSums(loadTrans != 0)
U2totalTrans <- colSums(U2Trans != 0) / colSums(loadTrans != 0)
U3totalTrans <- colSums(U3Trans != 0) / colSums(loadTrans != 0)
U4totalTrans <- colSums(U4Trans != 0) / colSums(loadTrans != 0)

allUsersTotalTrans <- U1totalTrans + U2totalTrans +U3totalTrans +U4totalTrans #value >1 means parallel actions 

U1Rot <- head(U1Rot,-1) # remove the last row 
U2Rot <- head(U2Rot,-1)
U3Rot <- head(U3Rot,-1)
U4Rot <- head(U4Rot,-1)

U1totalRot <- colSums(U1Rot != 0) / colSums(loadRot != 0)
U2totalRot <- colSums(U2Rot != 0) / colSums(loadRot != 0)
U3totalRot <- colSums(U3Rot != 0) / colSums(loadRot != 0)
U4totalRot <- colSums(U4Rot != 0) / colSums(loadRot != 0)

allUsersTotalRot <- U1totalRot + U2totalRot +U3totalRot +U4totalRot #value >1 means parallel actions 

U1Scale <- head(U1Scale,-1) # remove the last row 
U2Scale <- head(U2Scale,-1)
U3Scale <- head(U3Scale,-1)
U4Scale <- head(U4Scale,-1)

U1totalScale <- colSums(U1Scale != 0) / colSums(loadScale != 0)
U2totalScale <- colSums(U2Scale != 0) / colSums(loadScale != 0)
U3totalScale <- colSums(U3Scale != 0) / colSums(loadScale != 0)
U4totalScale <- colSums(U4Scale != 0) / colSums(loadScale != 0)

allUsersTotalScale <- U1totalScale + U2totalScale +U3totalScale +U4totalScale #value >1 means parallel actions 

U1Cam <- head(U1Cam,-1) # remove the last row 
U2Cam <- head(U2Cam,-1)
U3Cam <- head(U3Cam,-1)
U4Cam <- head(U4Cam,-1)

U1totalCam <- colSums(U1Cam != 0) / colSums(loadCam != 0)
U2totalCam <- colSums(U2Cam != 0) / colSums(loadCam != 0)
U3totalCam <- colSums(U3Cam != 0) / colSums(loadCam != 0)
U4totalCam <- colSums(U4Cam != 0) / colSums(loadCam != 0)

allUsersTotalCam <- U1totalCam + U2totalCam +U3totalCam +U4totalCam #value >1 means parallel actions 



taskTotalLoad <- colSums(loadTotal != 0) / nrow(loadTotal) # task total load

timeWorked <- colSums(loadTotal != 0) # get only the time worked


loadByTotalTime <- colSums(tidyUsers != 0) / nrow(tidyUsers) # load of each action of each users by total time
loadByWorkedTime <- colSums(tidyUsers != 0) / timeWorked # load of each action of each users only in the worked period

loadPerUser <- data.frame(U1totalTrans,U1totalRot,U1totalScale,U1totalCam,U2totalTrans,U2totalRot,U2totalScale,U2totalCam,U3totalTrans,U3totalRot,U3totalScale,U3totalCam,U4totalTrans,U4totalRot,U4totalScale,U4totalCam,allUsersTotalTrans,allUsersTotalRot,allUsersTotalScale,allUsersTotalCam)



