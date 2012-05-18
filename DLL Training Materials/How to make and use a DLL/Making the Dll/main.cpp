#define DLLEXPORT extern "C" __declspec(dllexport)

DLLEXPORT double addem (double a, double b){
	return a + b;  
}