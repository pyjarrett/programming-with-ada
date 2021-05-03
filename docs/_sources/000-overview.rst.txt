Overview
========

`Ada <https://learn.adacore.com/courses/intro-to-ada/index.html>`_
is an ISO-standardized programming language focusing on readability,
correctness and reliability.  Towards these goals it focuses on explicitness,
strong typing, compile time and run time constraints on types, and minimum
symbology.

Ada proves itself in reliability with a track record of nearly four decades of
usage in embedded, safety, and critical systems.  Over this timeframe,
Ada was updated three times, each time with a new
`Reference Manual <http://ada-auth.org/standards/rm12_w_tc1/RM-Final.pdf>`_,
a more in-depth
`Annotated Reference Manual <http://ada-auth.org/standards/aarm12_w_tc1/AA-Final.pdf>`_,
and a `Rationale <http://www.ada-europe.org/manuals/Rationale_2012.pdf>`_ document,
describing the reasoning for each feature.  Backing each of these changes is the
`Ada Conformity Assessment Test Suite (ACATS), <http://www.ada-auth.org/acats.html>`_
a battery of freely available tests to help Ada compilers or interprets properly
interpret the standard.  Ada 2012 takes reliability further, by supporting
inline usage of an Ada subset called 
`SPARK <https://learn.adacore.com/courses/intro-to-spark/index.html>`_,
which provides functional specification and static verification.

AdaCore provides `GNAT Community edition <https://www.adacore.com/download>`_,
an all-in-one download for multiple platforms.  This includes a cross-platform IDE
with a `code formatter <https://gcc.gnu.org/onlinedocs/gcc-11.1.0/gnat_ugn/Pretty-Printers-for-the-GNAT-runtime.html>`_,
`build system <https://github.com/AdaCore/gprbuild>`_,
documentation generator, integrated source control, auto-completion,
and a visual diff tool.  In addition to the Ada standard library, also
included is the `GNAT Components Collection (GNATColl) <https://github.com/AdaCore/gnatcoll-core>`_ 
which includes facilities such as JSON parsing,
`Ada Web Server <https://github.com/AdaCore/aws>`_ for providing browser
integration for your applications,
libgpr for manipulating GNAT project files, aunit for unit testing,
and `libadalang <https://github.com/AdaCore/libadalang>`_ for parsing and
semantic analysis of Ada code.

The emphasis on compile-time checks stops bugs at the earliest and cheapest point
in development preventing them from being added to the program.

The focus on correctness extends static typing past "put the square peg
in the square hole".  Ada prohibits implicit integer <->
float conversions, provides mechanisms to verify the range of values used
for floating point and integer types, and compiler injection of invariant checks
for user-defined types.  Lightweight semantic variations of existing types can also
be created which share the same underlying functions, but the compiler prevents
mixing these types with different meanings unless explicit casts are used.  This
adds meaning to even low level types like integers and floating point values,
preventing such mixing as accidentally adding a value meaning "joules" to one
meaning "meters".

Extensive runtime checks also help protect from memory safety errors (buffer overflows,
accessing unallocated memory, invalid array access) as well as logical errors
(range checks, pre/post condition checks, and type invariant checks).  Though
generally efficient and sometimes removable for checks which are known to never fail,
runtime checks can also be suppressed in specific areas, such as performance
critical areas, or throughout the program.

Ada supports built-in concurrency types for creating and synchronizing tasks to
take advantage of today's multiprocessor systems.  Ada 2012 also incorporates the
Ravenscar profile, a compiler pragma which restricts Ada to a subset for
deterministic behavior of the system for real-time systems.

Ada's history in high-reliability systems shows its focus on readability and
correctness.  As shown by its adoption by
`NVIDIA <https://www.adacore.com/company/partners/nvidia>`_ for security-critical
firmware, Ada 2012 remains an attractive option for future software development.
