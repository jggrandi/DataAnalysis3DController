# Extract Work Balance

Written by Jeronimo Grandi  
September 2016  

Maybe it is the ugliest script written in R ever. But, it works. How? I'll explain below. (You are lucky!)  

## Script Parameters:  

Input: Dataframe of one task.  
Output: Plot of the users work distribution for each transformation;  


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


