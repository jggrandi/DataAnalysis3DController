# Extract Work Balance

Written by Jeronimo Grandi  
September 2016  

Maybe it is the ugliest script written in R ever. But, it works. How? I'll explain below. (You are lucky!)  

## Script Parameters:  

Input: Dataframe of one task.  
Output: Load by task total time of each user;  
		Load by worked time of each user;  
        Load per transformation of each user;  


## Steps:  

0. Before start:
  1. It uses dplyr and tidyr. So, you have to install them. (ex: package.install("dplyr"))  
  2. Go to Session->Set Working Directory->Choose Directory. Select the folder Analysis->R  
  3. In the tab Environment, Click in Open. Select .RData  
  4. In the tab History, Click in Open. Select .Rhistory  
1. If it's the first time that you are running this script:  
  1. Search for all comments "if first" and uncomment them;  
  2. Serach for all comments "else add to the exiting" and comment them;  
  3. You have to change "_taskX" (X is the number of the task) with the number of the task of the file loaded;  
2. After the first run, do the opposite. Comment "if first" and uncomment "else..."
3. The script works in a specific way for different quantity of users. So, define it beforehand in the variable "qntusers"
4. After qntusers defined, the in the line  "users <- select(..." of the if whith the qntusers defined, specify the data frame you want.  
  1. Pay atention to load the file with the same quantity of users as defined  
  2. The data frame has to be previously loaded in the Environment  
5. Run the script.  
  1. I usually select the lines that I want to run and after run them with Ctrl+Enter  



