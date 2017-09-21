#include <iostream>
#include <cstdio>
#include <cstring>

#include "big-endian.h"

using namespace std;

int main()
  {
    be_int32_t x;
    int32_t a;
    int32_t b;

    x = 0x12345678;     // store as big-endian
    a = x;              // automatic conversion to native byte order
    memcpy(&b, &x, 4);  // copy as raw data

    cout << "sizeof(int32_t) = " << sizeof(int32_t) << endl;
    cout << "sizeof(be_int32_t) = " << sizeof(be_int32_t) << endl;
    cout << hex << "a = " << a << " b = " << b << endl;
    cout << hex << "[cout]   x = " << x << endl;  // automatic conversion
    printf("[printf] x = %lx\n", x);              // no automatic conversion
    printf("[printf] (int32_t) x = %lx\n", (int32_t) x);
                                                  // manual conversion

    return 0;
  }
