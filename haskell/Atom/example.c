#include <stdbool.h>
#include <stdint.h>


#include <stdlib.h>
#include <stdio.h>
unsigned long int a;
unsigned long int b;
unsigned long int x;
unsigned char running = 1;


static uint64_t __global_clock = 0;



static const uint32_t __coverage_len = 1;
static uint32_t __coverage[1] = {0};
static uint32_t __coverage_index = 0;
struct {  /* state */
  struct {  /* example */
  } example;
} state =
{  /* state */
  {  /* example */

  }
};

/* example.a_minus_b */
static void __r0() {
  uint32_t __0 = b;
  uint32_t __1 = a;
  bool __2 = __0 < __1;
  uint32_t __3 = __1 - __0;
  uint32_t __4 = __2 ? __3 : __1;
  if (__2) {
    __coverage[0] = __coverage[0] | (1 << 0);
  }
  a = __4;
}

/* example.b_minus_a */
static void __r1() {
  uint32_t __0 = a;
  uint32_t __1 = b;
  bool __2 = __0 < __1;
  uint32_t __3 = __1 - __0;
  uint32_t __4 = __2 ? __3 : __1;
  if (__2) {
    __coverage[0] = __coverage[0] | (1 << 1);
  }
  b = __4;
}

/* example.stop */
static void __r2() {
  uint32_t __0 = a;
  uint32_t __1 = b;
  bool __2 = __0 == __1;
  bool __3 = ! __2;
  bool __4 = running;
  bool __5 = __3 && __4;
  if (__2) {
    __coverage[0] = __coverage[0] | (1 << 2);
  }
  running = __5;
}


static void __assertion_checks() {
}


void example()
{

  {
    static uint8_t __scheduling_clock = 0;
    if (__scheduling_clock == 0) {
      __assertion_checks(); __r0();  /* example.a_minus_b */
      __assertion_checks(); __r1();  /* example.b_minus_a */
      __assertion_checks(); __r2();  /* example.stop */
      __scheduling_clock = 0;
    }
    else {
      __scheduling_clock = __scheduling_clock - 1;
    }
  }

  __global_clock = __global_clock + 1;

}

int main(int argc, char* argv[]) {
  if (argc < 3) {
    printf("usage: gcd <num1> <num2>\n");
  }
  else {
    a = atoi(argv[1]);
    b = atoi(argv[2]);
    printf("Computing the GCD of %lu and %lu...\n", a, b);
    while(running) {
      example();
      printf("iteration:  a = %lu  b = %lu\n", a, b);
    }
    printf("GCD result: %lu\n", a);
  }
  return 0;
}

