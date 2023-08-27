#include "thetalib.h"


int power(int base, int a)
{
int result;
if(a != 0)
{
result = base * power(base, a - 1);
}
else
{
result = 1;
}
return result;
}

void main(){
int base, a, result;
writeStr("Enter base number: ");
base = readInteger();
writeStr("Enter power number(positive integer): ");
a = readInteger();
result = power(base, a);
writeInteger(result);
return;
}
