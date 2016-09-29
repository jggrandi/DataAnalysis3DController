# Maybe it is not as ugly as the tidyDataUserActions script, but still be very ugly. I'll explain below how it works. (You are lucky!)
# First, let me introduce myself. I'm Jeronimo and I'll be your guide. 

# Input: Dataframe of one task.
# Output: Roles during the task for each user;

# Roles code:
# 3 - Tranlation      8 - Trans+Rot         12- Rot+Scale
# 5 - Rotation        10- Trans+Scale       16- Rot+Cam
# 7 - Scale           14- Trans+Cam         18- Scale+Cam
# 11- Cam Rot


# 0 Before start:
# 0.0 It uses dplyr and tidyr. So, you have to install them. (ex: package.install("dplyr"))
# 0.1 Go to Session->Set Working Directory->Choose Directory. Select the folder Analysis->R
# 0.2 In the tab Environment, Click in Open. Select .RData
# 0.3 In the tab History, Click in Open. Select .Rhistory
# 1. The script works in a specific way for different quantity of users. So, define it beforehand in the variable "qntusers"
# 2. After qntusers defined, the in the line  "users <- select(..." of the if whith the qntusers defined, specify the data frame you want.
# 2.1 Pay atention to load the file with the same quantity of users as defined
# 2.2 The data frame has to be previously loaded in the Environment
# 3. Run the script.
# 3.1 I usually select the lines that I want to run and after run them with Ctrl+Enter


qntusers <- 4
if(qntusers == 1)
  users <- select(X23.1.Task4.2016.06.10.18.37.57,ends_with(".1"), -starts_with("User"),-starts_with("Con")) #select only user action in object transformation 
if(qntusers == 2)
  users <- select(X22.2.Task4.2016.06.10.18.16.35,ends_with(".1"),ends_with(".2"), -starts_with("User"),-starts_with("Con")) #select only user action in object transformation 
if(qntusers == 3)
  users <- select(X18.3.Task4.2016.06.09.10.22.24,ends_with(".1"),ends_with(".2"), ends_with(".3"), -starts_with("User"),-starts_with("Con")) #select only user action in object transformation 
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
  tidyUsers$U1TransAsd<- lapply(tidyUsers$U1Trans, function(x) ifelse(x >0, 3, 0))
  tidyUsers$U1RotAsd<- lapply(tidyUsers$U1Rot, function(x) ifelse(x >0, 5, 0))
  tidyUsers$U1ScaleAsd<- lapply(tidyUsers$U1Scale, function(x) ifelse(x >0, 7, 0))
  tidyUsers$U1CamAsd<- lapply(tidyUsers$U1Cam, function(x) ifelse(x >0, 11, 0))
}

if(qntusers == 2 |qntusers == 3 | qntusers == 4){
  tidyUsers$U2TransAsd<- lapply(tidyUsers$U2Trans, function(x) ifelse(x >0, 3, 0))
  tidyUsers$U2RotAsd<- lapply(tidyUsers$U2Rot, function(x) ifelse(x >0, 5, 0))
  tidyUsers$U2ScaleAsd<- lapply(tidyUsers$U2Scale, function(x) ifelse(x >0, 7, 0))
  tidyUsers$U2CamAsd<- lapply(tidyUsers$U2Cam, function(x) ifelse(x >0, 11, 0))
}


if(qntusers == 3 | qntusers == 4){
  tidyUsers$U3TransAsd<- lapply(tidyUsers$U3Trans, function(x) ifelse(x >0, 3, 0))
  tidyUsers$U3RotAsd<- lapply(tidyUsers$U3Rot, function(x) ifelse(x >0, 5, 0))
  tidyUsers$U3ScaleAsd<- lapply(tidyUsers$U3Scale, function(x) ifelse(x >0, 7, 0))
  tidyUsers$U3CamAsd<- lapply(tidyUsers$U3Cam, function(x) ifelse(x >0, 11, 0))
}

if(qntusers == 4){
  tidyUsers$U4TransAsd<- lapply(tidyUsers$U4Trans, function(x) ifelse(x >0, 3, 0))
  tidyUsers$U4RotAsd<- lapply(tidyUsers$U4Rot, function(x) ifelse(x >0, 5, 0))
  tidyUsers$U4ScaleAsd<- lapply(tidyUsers$U4Scale, function(x) ifelse(x >0, 7, 0))
  tidyUsers$U4CamAsd<- lapply(tidyUsers$U4Cam, function(x) ifelse(x >0, 11, 0))
}

if(qntusers == 1 | qntusers == 2 |qntusers == 3 | qntusers == 4){
  user1 <- tidyUsers[c("U1TransAsd","U1RotAsd","U1ScaleAsd","U1CamAsd")]
  asd2 <- apply(user1, 1, function(x) any(x) && x[as.logical(x)][1] >0)
  user1 <- user1[asd2,]
  U1 <- mutate(user1, SUM = as.numeric(U1RotAsd)+ as.numeric(U1TransAsd)+ as.numeric(U1ScaleAsd)+ as.numeric(U1CamAsd))  
  U1 <- data.frame(t(U1$SUM))
  U1$zero <- 0 # add zero in the end of the line
  row.names(U1) <- "1"
  write.table( U1, sep=" ",  col.names=FALSE,file = "AllUsersRolesRaw.csv",na = " ", append = TRUE)
}

if(qntusers == 2 |qntusers == 3 | qntusers == 4)
{
  user2 <- tidyUsers[c("U2TransAsd","U2RotAsd","U2ScaleAsd","U2CamAsd")]
  asd2 <- apply(user2, 1, function(x) any(x) && x[as.logical(x)][1] >0)
  user2 <- user2[asd2,]
  U2 <- mutate(user2, SUM = as.numeric(U2RotAsd)+ as.numeric(U2TransAsd)+ as.numeric(U2ScaleAsd)+ as.numeric(U2CamAsd))  
  U2 <- data.frame(t(U2$SUM))
  U2$zero <- 0 # add zero in the end of the line
  row.names(U2) <- "2"
  write.table( U2, sep=" ",  col.names=FALSE,file = "AllUsersRolesRaw.csv",na = " ", append = TRUE)
}

if(qntusers == 3 | qntusers == 4){
  user3 <- tidyUsers[c("U3TransAsd","U3RotAsd","U3ScaleAsd","U3CamAsd")]
  asd2 <- apply(user3, 1, function(x) any(x) && x[as.logical(x)][1] >0)
  user3 <- user3[asd2,]
  U3 <- mutate(user3, SUM = as.numeric(U3RotAsd)+ as.numeric(U3TransAsd)+ as.numeric(U3ScaleAsd)+ as.numeric(U3CamAsd))
  U3 <- data.frame(t(U3$SUM))
  U3$zero <- 0  # add zero in the end of the line
  row.names(U3) <- "3"
  write.table( U3, sep=" ",  col.names=FALSE,file = "AllUsersRolesRaw.csv",na = " ", append = TRUE)
}

if(qntusers == 4){
  user4 <- tidyUsers[c("U4TransAsd","U4RotAsd","U4ScaleAsd","U4CamAsd")]
  asd2 <- apply(user4, 1, function(x) any(x) && x[as.logical(x)][1] >0)
  user4 <- user4[asd2,]
  U4 <- mutate(user4, SUM = as.numeric(U4RotAsd)+ as.numeric(U4TransAsd)+ as.numeric(U4ScaleAsd)+ as.numeric(U4CamAsd))
  U4 <- data.frame(t(U4$SUM))
  U4$zero <- 0  # add zero in the end of the line
  row.names(U4) <- "4"
  write.table( U4, sep=" ",  col.names=FALSE,file = "AllUsersRolesRaw.csv",na = " ", append = TRUE)
}

# if(qntusers == 1)
#   UALL <- bind_rows(U1)
# if(qntusers == 2)
#   UALL <- bind_rows(U1,U2)
# if(qntusers == 3)
#   UALL <- bind_rows(U1,U2,U3)
# if(qntusers == 4)
#   UALL <- bind_rows(U1,U2,U3,U4)


#write.csv(t(aaa[5]),file = "aaa.csv")

#write.csv(UALL,file = "aaa.csv")
#write.table( UALL, sep=" ",  col.names=FALSE,file = "AllUsersRolesRaw.csv",na = " ")
