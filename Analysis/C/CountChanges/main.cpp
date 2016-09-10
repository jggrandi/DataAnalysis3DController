#include <iostream>

using namespace std;

int main(){
    int considerChange = 4;
    int stillSameRole = 0;
    int input = -1;
    int valueNow = -1;
    int countChanges = 0;
    int member;
    int count = 0;
    int temp = -1;
    while(member != 99){
        cin >> member;
        if(member == 99) break;
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
        cout << "Changes by Member " << member << ": " << countChanges << endl;
        countChanges = 0;
        count = 0;
        temp = -1;
    }
    cout << "END" << endl;
    return 0;

}
