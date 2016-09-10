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
