#include <iostream>
using namespace std;

#define DLLEXPORT extern "C" __declspec(dllexport)
DLLEXPORT double addem(double, double);

void main(){
	cout << addem(1, 2) << endl;
}
