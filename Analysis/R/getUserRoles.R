#To get user role changes

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

if(qntusers == 1 | qntusers == 2 |qntusers == 3 | qntusers == 4){
  tidyUsers$U1TransAsd<- lapply(tidyUsers$U1Trans, function(x) ifelse(x >0, 1, 0))
  tidyUsers$U1RotAsd<- lapply(tidyUsers$U1Rot, function(x) ifelse(x >0, 2, 0))
  tidyUsers$U1ScaleAsd<- lapply(tidyUsers$U1Scale, function(x) ifelse(x >0, 3, 0))
  tidyUsers$U1CamAsd<- lapply(tidyUsers$U1Cam, function(x) ifelse(x >0, 4, 0))
}

if(qntusers == 2 |qntusers == 3 | qntusers == 4){
  tidyUsers$U2TransAsd<- lapply(tidyUsers$U2Trans, function(x) ifelse(x >0, 1, 0))
  tidyUsers$U2RotAsd<- lapply(tidyUsers$U2Rot, function(x) ifelse(x >0, 2, 0))
  tidyUsers$U2ScaleAsd<- lapply(tidyUsers$U2Scale, function(x) ifelse(x >0, 3, 0))
  tidyUsers$U2CamAsd<- lapply(tidyUsers$U2Cam, function(x) ifelse(x >0, 4, 0))
}


if(qntusers == 3 | qntusers == 4){
  tidyUsers$U3TransAsd<- lapply(tidyUsers$U3Trans, function(x) ifelse(x >0, 1, 0))
  tidyUsers$U3RotAsd<- lapply(tidyUsers$U3Rot, function(x) ifelse(x >0, 2, 0))
  tidyUsers$U3ScaleAsd<- lapply(tidyUsers$U3Scale, function(x) ifelse(x >0, 3, 0))
  tidyUsers$U3CamAsd<- lapply(tidyUsers$U3Cam, function(x) ifelse(x >0, 4, 0))
}

if(qntusers == 4){
  tidyUsers$U4TransAsd<- lapply(tidyUsers$U4Trans, function(x) ifelse(x >0, 1, 0))
  tidyUsers$U4RotAsd<- lapply(tidyUsers$U4Rot, function(x) ifelse(x >0, 2, 0))
  tidyUsers$U4ScaleAsd<- lapply(tidyUsers$U4Scale, function(x) ifelse(x >0, 3, 0))
  tidyUsers$U4CamAsd<- lapply(tidyUsers$U4Cam, function(x) ifelse(x >0, 4, 0))
}

if(qntusers == 1 | qntusers == 2 |qntusers == 3 | qntusers == 4)
  user1 <- tidyUsers[17:20]
if(qntusers == 2 |qntusers == 3 | qntusers == 4)
  user1 <- tidyUsers[21:24]
if(qntusers == 3 | qntusers == 4)
  user1 <- tidyUsers[25:28]
if(qntusers == 4)
  user1 <- tidyUsers[29:32]

asd2 <- apply(user1, 1, function(x) any(x) && x[as.logical(x)][1] >0)
desired.result <- asd[asd2,]
aaa <- mutate(desired.result, SUM = as.numeric(U1RotAsd)+ as.numeric(U1TransAsd)+ as.numeric(U1ScaleAsd)+ as.numeric(U1CamAsd))

#write.csv(t(aaa[5]),file = "aaa.csv")
