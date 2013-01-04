#include <gc/gc.h>
#include <gc/gc_typed.h>
#include <sys/mman.h> /* mprotect */

#if defined(__linux__)
#pragma for Ada "sys/mman.h" include "bits/mman.h"
#endif
