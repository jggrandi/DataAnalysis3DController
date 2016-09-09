#include <iostream>

using namespace std;

int main(){
    int considerChange = 4;
    int stillSameRole = 0;
    int input = -1;
    int valueNow = 0;
    int countChanges = 0;
    int count = 0;
    int toCompare[3];
    while(input != 0){
        cin >> input;

        toCompare[count] = input;
        count++;
        if(count >= 3)
                count = 0;

            if(input != toCompare[2] && input == toCompare[1] && input == toCompare[0])
            {
             countChanges++;

            }

    }
    cout << "Changes: " << countChanges << endl;
    return 0;

}
