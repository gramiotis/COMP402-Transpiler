#include "thetalib.h"


int checkPrimeNumber(int n)
{
int j, flag;
flag = 1;
for(int j = 2; j <= n / 2; j++)
{
if(n % j == 0)
{
flag = 0;
break;
}
}
return flag;
}

void main(){
int n1, n2, i, flag;
writeStr("Enter first positive integers: ");
n1 = readInteger();
writeStr("Enter second positive integers: ");
n2 = readInteger();
if(n1 > n2)
{
n1 = n1 + n2;
n2 = n1 - n2;
n1 = n1 - n2;
}
writeStr("Prime numbers between n1 and n2 are: ");
for(int i = n1 + 1; i <= n2; i++)
{
flag = checkPrimeNumber(i);
if(flag == 1)
{
writeInteger(i);
}
}
return;
}
