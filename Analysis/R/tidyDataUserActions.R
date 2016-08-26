#tidy3Task3 <- subset(`3-Task3`, select=c(Time,Translation.X.1,Translation.Y.1,Translation.Z.1,Rotation.X.1,Rotation.Y.1,Rotation.Z.1,Rotation.W.1,Scalling.1,Translation.X.2,Translation.Y.2,Translation.Z.2,Rotation.X.2,Rotation.Y.2,Rotation.Z.2,Rotation.W.2,Scalling.2,Translation.X.3,Translation.Y.3,Translation.Z.3,Rotation.X.3,Rotation.Y.3,Rotation.Z.3,Rotation.W.3,Scalling.3,Translation.X.4,Translation.Y.4,Translation.Z.4,Rotation.X.4,Rotation.Y.4,Rotation.Z.4,Rotation.W.4,Scalling.4))
#tidyUsers <- tidyUsers[-c(3253),] ## to remove a row
#nrow(tidyUsers)

users <- select(task,ends_with(".1"),ends_with(".2"), ends_with(".3"),ends_with(".4"), -starts_with("User"),-starts_with("Con"))
users[users>0.999999] <- 1 # remove the noise in the data 

U1Trans <- transmute(users, U1Trans = sqrt((Translation.X.1 * Translation.X.1) + (Translation.Y.1 * Translation.Y.1) + (Translation.Z.1 * Translation.Z.1)))
U2Trans <- transmute(users, U2Trans = sqrt((Translation.X.2 * Translation.X.2) + (Translation.Y.2 * Translation.Y.2) + (Translation.Z.2 * Translation.Z.2)))
U3Trans <- transmute(users, U3Trans = sqrt((Translation.X.3 * Translation.X.3) + (Translation.Y.3 * Translation.Y.3) + (Translation.Z.3 * Translation.Z.3)))
U4Trans <- transmute(users, U4Trans = sqrt((Translation.X.4 * Translation.X.4) + (Translation.Y.4 * Translation.Y.4) + (Translation.Z.4 * Translation.Z.4)))

rad2deg <- function(rad) {(rad * 180) / (pi)}

U1Rot <- transmute(users, U1Rot = rad2deg(2*acos(Rotation.W.1)))
U2Rot <- transmute(users, U2Rot = rad2deg(2*acos(Rotation.W.2)))
U3Rot <- transmute(users, U3Rot = rad2deg(2*acos(Rotation.W.3)))
U4Rot <- transmute(users, U4Rot = rad2deg(2*acos(Rotation.W.4)))

U1Scale <- transmute(users, U1Scale = abs(Scalling.1-1))
U2Scale <- transmute(users, U2Scale = abs(Scalling.2-1))
U3Scale <- transmute(users, U3Scale = abs(Scalling.3-1))
U4Scale <- transmute(users, U4Scale = abs(Scalling.4-1))

U1RotCam <- transmute(users, U1Cam = rad2deg(2*acos(Rotation.W.1)))
U2RotCam <- transmute(users, U2Cam = rad2deg(2*acos(Rotation.W.2)))
U3RotCam <- transmute(users, U3Cam = rad2deg(2*acos(Rotation.W.3)))
U4RotCam <- transmute(users, U4Cam = rad2deg(2*acos(Rotation.W.4)))

tidyUsers <- data.frame(U1Trans,U1Rot,U1Scale,U1RotCam,U2Trans,U2Rot,U2Scale,U2RotCam,U3Trans,U3Rot,U3Scale,U3RotCam,U4Trans,U4Rot,U4Scale,U4RotCam)

loadTrans <- transmute(tidyUsers, loadTrans = U1Trans + U2Trans + U3Trans + U4Trans)
loadScale <- transmute(tidyUsers, loadScale = U1Scale + U2Scale + U3Scale + U4Scale)
loadRot <- transmute(tidyUsers, loadRot = U1Rot + U2Rot + U3Rot + U4Rot)
loadCam <- transmute(tidyUsers, loadCam = U1Rot.1 + U2Rot.1 + U3Rot.1 + U4Rot.1)
loadTotal <- loadTrans + loadRot + loadScale + loadCam

colSums(loadTotal != 0) / nrow(loadTotal) # task total load

timeWorked <- colSums(loadTotal != 0) # get only the time worked

colSums(tidyUsers != 0) / nrow(tidyUsers) # load by total time
colSums(tidyUsers != 0) / timeWorked # load of each action of each users only in the worked period




