/* Count the user roles changes
Input: The user roles (Output of getUserRoles.R script ) 
Output: How many times an user changed role

To feed this program use (for example):
 cat ../../R/ExtractUserRoles/AllUsersRolesRaw1-5.csv | ./CountChanges

 Written by Jerônimo Grandi.
 September 2016
*/


#include <iostream>

using namespace std;

int main(){
    int considerChange = 4;
    int input, valueNow = -1;
    int countChanges,userID,count,temp=0;

    cout << "UserID;Changes" <<endl;
    while(userID != 99){
        cin >> userID;
        if(userID == 99) break;
        while(input != 0){
            cin >> input;
            if (input == 0){
                input = -1; break;
            }

            if(input != temp ){
                temp = input;
                count = 0;
            }
            else if(temp != valueNow)
                count++;
            else
                count = 0;

            if(count == considerChange-1){
                valueNow = input;
                countChanges++;
            }

        }
        cout << userID << ";" << countChanges << endl;
        countChanges = 0;
        count = 0;
        temp = -1;
        valueNow = -1;
    }

    return 0;

}
