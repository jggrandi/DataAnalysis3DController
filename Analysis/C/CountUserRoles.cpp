#include <iostream>

using namespace std;

int main(){
	int considerChange = 4;
	int stillSameRole = 0;
	int input;
	int valueNow = 0;
	int countChanges = 0;
	while(input != 0){
		cin >> input;
		if(input != valueNow){
			stillSameRole++;
			if(stillSameRole == considerChange){
				valueNow = input;
				countChanges++;
				stillSameRole = 0;
			}
		}
		stillSameRole = 0;

	}
	cout << "Changes: " << countChanges << endl;
	return 0;

}
