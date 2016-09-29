# Extract Users Roles

Written by Jeronimo Grandi  
September 2016  

Maybe it is not as ugly as the tidyDataUserActions script, but still be very ugly. I'll explain below how it works. (You are lucky!)  


## Script Parameters:  

Input: Dataframe of one task.
Output: Roles during the task for each user;
 
Roles code:
3 - Tranlation      8 - Trans+Rot         12- Rot+Scale  
5 - Rotation        10- Trans+Scale       16- Rot+Cam  
7 - Scale           14- Trans+Cam         18- Scale+Cam  
11- Cam Rot  

## Steps:  

0. Before start:  
  1. It uses dplyr and tidyr. So, you have to install them. (ex: package.install("dplyr"))  
  2. Go to Session->Set Working Directory->Choose Directory. Select the folder Analysis->R  
  3. In the tab Environment, Click in Open. Select .RData  
  4. In the tab History, Click in Open. Select .Rhistory  
1. The script works in a specific way for different quantity of users. So, define it beforehand in the variable "qntusers"  
2. After qntusers defined, the in the line  "users <- select(..." of the if whith the qntusers defined, specify the data frame you want.  
  1. Pay atention to load the file with the same quantity of users as defined  
  2. The data frame has to be previously loaded in the Environment  
3. Run the script.  
  1. I usually select the lines that I want to run and after run them with Ctrl+Enter  