#include <gc/gc.h>
#include <gc/gc_typed.h>
#include <sys/mman.h> /* mprotect */

#if defined(__linux__)
#if !__GLIBC_PREREQ(2, 18)
#pragma for Ada "sys/mman.h" include "bits/mman.h"
#else
#pragma for Ada "sys/mman.h" include "bits/mman-linux.h"
#endif
#endif

#if defined(GC_RETURN_ADDR)
#undef GC_RETURN_ADDR
#endif
#if defined(GC_RETURN_ADDR_PARENT)
#undef GC_RETURN_ADDR_PARENT
#endif
