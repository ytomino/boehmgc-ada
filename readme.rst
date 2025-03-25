Boehm GC interface library for gcc-Ada (GNAT)
=============================================

What's this?
------------

Ada binding to the Boehm-Demers-Weiser Garbage Collector.

Prerequisites
-------------

GCC >= 4.7
 https://gcc.gnu.org/
Boehm-Demers-Weiser Garbage Collector >= 7.2
 https://www.hboehm.info/gc/
headmaster
 https://github.com/ytomino/headmaster

Usage
-----

1. Prepare the translated headers.

   A. Translate the C headers with headmaster. ::

       $ headmaster --to ada -p -D import-dir boehmgc-ada/source/import.h
      
      However, it may not work well in your environment.
      The plan B is recommended.

   B. Download them from `pre-translated headers page`_.

2. Add the source directories of boehmgc-ada and the translated headers
   to search path for gnatmake. ::

    $ gnatmake -Iboehmgc-ada/source -Iimport-dir your_main.adb
   
   Or please write .gpr file for your environment.

Build examples
--------------

1. Link the translated headers to `examples/import`. ::

    $ mkdir boehmgc-ada/examples/import
    $ ln -s $PWD/import-dir boehmgc-ada/examples/import/$(gcc -dumpmachine)
   
   If this step is omitted, headmaster will be used.

2. Build them. ::

    $ make -C boehmgc-ada/examples

Limitations
-----------

Multithreading is unsupported.

License
-------

It is licensed under the New BSD License, see below.
Also, please apply the license of Boehm-Demers-Weiser Garbage Collector when
static linking.

**license of boehmgc-ada** ::

 Copyright 2010-2025 YT. All rights reserved.
 
 Redistribution and use in source and binary forms, with or without
 modification, are permitted provided that the following conditions
 are met:
 1. Redistributions of source code must retain the above copyright
    notice, this list of conditions and the following disclaimer.
 2. Redistributions in binary form must reproduce the above copyright
    notice, this list of conditions and the following disclaimer in the
    documentation and/or other materials provided with the distribution.
 
 THIS SOFTWARE IS PROVIDED BY THE AUTHOR(S) ``AS IS'' AND ANY EXPRESS OR
 IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
 IN NO EVENT SHALL THE AUTHOR(S) BE LIABLE FOR ANY DIRECT, INDIRECT,
 INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

**license of Boehm-Demers-Weiser Garbage Collector** ::

 MIT-style License
 
 Copyright (c) 1988-1989 Hans-J. Boehm, Alan J. Demers
 Copyright (c) 1991-1996 by Xerox Corporation.  All rights reserved.
 Copyright (c) 1996-1999 by Silicon Graphics.  All rights reserved.
 Copyright (c) 1998 by Fergus Henderson.  All rights reserved.
 Copyright (c) 1999-2001 by Red Hat, Inc.  All rights reserved.
 Copyright (c) 1999-2011 Hewlett-Packard Development Company, L.P.
 Copyright (c) 2004-2005 Andrei Polushin
 Copyright (c) 2007 Free Software Foundation, Inc.
 Copyright (c) 2008-2022 Ivan Maidanski
 Copyright (c) 2011 Ludovic Courtes
 Copyright (c) 2018 Petter A. Urkedal
 
 
 THIS MATERIAL IS PROVIDED AS IS, WITH ABSOLUTELY NO WARRANTY EXPRESSED
 OR IMPLIED.  ANY USE IS AT YOUR OWN RISK.
 
 Permission is hereby granted to use or copy this program
 for any purpose, provided the above notices are retained on all copies.
 Permission to modify the code and to distribute modified code is granted,
 provided the above notices are retained, and a notice that the code was
 modified is included with the above copyright notice.
 
 
 Several files (gc/gc_allocator.h, extra/msvc_dbg.c) come with slightly
 different licenses, though they are all similar in spirit (the exact
 licensing terms are given at the beginning of the corresponding source file).
 
 A few of the files needed to use the GNU-style build procedure come with
 a modified GPL license that appears not to significantly restrict use of
 the collector, though use of those files for a purpose other than building
 the collector may require the resulting code to be covered by the GPL.

.. _`pre-translated headers page`: https://github.com/ytomino/boehmgc-ada/wiki/Pre-translated-headers
