####################
Four Months with Ada
####################

For the last four months I've been learning and writing Ada 2012.
This is a more in-depth examination of the language.
For those unfamiliar with Ada terms, in general, I'll using the general programming vernacular, rather than the Ada-specific definition of terms.

**************************************************
A Free-Function Focused C++, or a Strongly Typed C
**************************************************

Ada focuses on creating packages of code which contain types and related functions which use those types.
Since it lacks implicit casts, it uses function overloading as a key design element.

The language writes like a strongly typed C with a strong emphasis on defining types and then functions for those types.
There's no preprocessor, so instead of ``#include``, there are "packages" which contain related types and functions.
These also act as namespaces for functions instead of types themselves, so no distinction exists between between how "free functions", "class functions", and "member functions" are declared, they're all just functions within a package.

Packages can contain startup code executed by the "environment task" prior to entering the main procedure for initialization.

***************
Focus on Intent
***************

Parameter Modes
===============

Ada source focuses on describing intent and modeling semantics.
For example, function parameters can be either ``in``, ``out``, or both.  ``in`` parameters are readonly, and while you can force passing by reference via specifics in the language, you often just ignore how this happens.

Describing semantics goes all the way into primitive types, and the rules are
consistent between primitive and user-defined types.
Lightweight types with domain-specific meaning can be created, and along with no implicit casting, prevents mishandling of semantics on primitive types.
Interfaces rarely use ``Integer`` or ``Float`` directly, instead you'll find semantic versions ("derived types") created such as "Miles" or "Kilometers".

.. code-block :: Ada

    type Kilometers is new Natural;
    type Miles is new Natural;

    K : Kilometers := 10;
    M : Miles := 50;

    M := K;  -- Compile error!

This seems bizarre but it plays into the extensive use of function overloading in the language.
With implicit casting and with function overloading, creating functions and transforms of types, even numerical is straightforward.
Compile and runtime checks provide bounds-checking and numerical types can have their bounds constrained to "known good" values.

Enumeration Types
=================

Enumeration types have first class support, with operations for ``'First`` and ``'Last`` of the set of values and ``'Pred`` (predecessor) and ``'Succ`` for individual values.
Enumeration over all values and conversions to and from strings and integers get provided for free.

Pre- and Post-Conditions
========================

Ada adds built-in support for pre and post conditions, through the use of language aspects.
This is a "killer feature" of Ada 2012, on top of all of the other type checking and safety checking, pre and post conditions get provided as part of the specification of the function.
Clients can see it as part of the interface and the compiler inserts runtime checks if enabled.
A lot of languages have an assertion mechanism which often effectively gets used for these checks, but a built-in way of doing this which shows up in the interface is a major game changer.
I found that adding pre and post condition checks during debugging to be a very effective tool.

.. code-block :: Ada

    function Merge (A, B : Context_Match) return Context_Match with
        Pre  => Is_Valid (A) and then Is_Valid (B),
        Post => Is_Valid (Merge'Result);

Types which expose no private state can also have type invariants which are checked prior to usage as function arguments and after assignments.

.. code-block :: Ada

   type Spinner is record
       Ticks_Per_Move : Positive;
       Ticks          : Natural;
       State          : Spinner_State;
       Style          : Spinner_Style;
   end record with
       Type_Invariant => Ticks < Ticks_Per_Move;

Discriminants
=============

Another strange concept which I hadn't seen before, is that types can be "parameterized" at runtime using what is called a "discriminant."
It's a sort of read-only constant field which is part of the record.

This read only value becomes part of the type and whether explicitly described or not affects type checking.
An extremely commonly used one is with the built-in String type in which the string's length is given by the discriminant.

Protected Objects
=================

Protected objects coordinate concurrent access to shared state.
The control can also include arbitrarily complex conditionals as well, such as not allowing any writers when readers exist, or blocking any more readers when a writer is waiting.

Tasks
=====

Tasks provide concurrent execution.
Additionally, they have special procedures called "entries" which can be "accepted" by a related task during its flow of execution to synchronize (rendezvous) with other tasks and share data at these points.

Tasks run concurrently in the block in which they're declared, and the block will not exit until the task finishes or terminates, unless it is allocated on the heap.

Both single instance and instantiable versions of protected objects and tasks can be created.

Generics
========

Generic packages or functions must be explicitly instantiated for use.
This eliminates the debate of angled brackes (<>) versus square brackets for generics ([]), but leads to additional names being created.
The benefit of this is making their usage, and hence their cost, explicit, at the expense of verboseness.

******************
Low Level Controls
******************

Accessing C functions and compiler intrinsics is straightforward.
You create a declaration of the subprogram and then describe where it comes from using aspects or the ``Import`` pragma.
Using representation clauses makes it trivial to match C struct layout or binary formats such as for files.
Since the usage is the same as with an Ada function, imported functions can be replaced easily if needed.
Inline assembler is also available, but due to the lack of a preprocessor, the build system (gpr) is leveraged to choose the appropriate definition (body) file to compile.


*******************
Hurdles to Adoption
*******************

License Confusion
=================

The material and information provided on this site is for general information
purposesly only.

I am not a not a lawyer and I am not providing legal advice.
The information here is for informational purposes only and not for the purpose of providing legal advice.
You should contact a qualified You should consult with a qualified and licensed lawyer


*****
Alire
*****

Alire simplifies Ada development significantly, by simplifying project generation,
building, running, and dependency management.

It borrows heavily from Cargo.
If good arts borrow, and great artists steal, then Ada is on par with Michaelangelo.
In its quest for modernizing, many concepts of Rust's cargo are being built in to a similar tool for Ada called Alire.
This tool only went to 1.0 since I've been working with Ada, but iot simplifies building and editing considerably.
Getting dependencies and setting up projects also gets streamlined significantly.
The toolchain itself will eventually be integrated into this tool, so it will become a sort of one-stop shop for downloading things for Ada.

There's a bit of jank to get a crate into the manager.
Every package and version update requires manual approval for now.
While this prevents name squatting and ensures existing libraries can get their appropriate names,
it can be frustrating waiting for approval even though things usually get approved quickly.

You can use local unpublished versions as a dependency, which helps when developing libraries,
and keeps you moving if you're waiting that day for approval.

Overall, Alire makes it incredibly easy to split up your project into multiple libraries.

Right now, Ada is a playground for library and tool writers.  It's a mature language with
excellent C compatibility in need of a lot of basic libraries.  In addition, it provides
the means to create, distribute and use formally verified libraries.  This isn't some
hypothetical pipe dream anymore.  This means formally verified programs are here for
the main stream.  adaCore does have the highest level selvers bheind a paywall of paid support,
but "silver" level programs and libraries are here.


Ada suffers from a lack of familiarity for many programmers due to being a Pascal family language and also its peculiar, but very specific vocabulary.
It's not as obscure to learn as you think it would be.
The usage of keywords over punctuation helps ease many problems of dealign with an unfamiliar language.
While this helps with googling, a lot of uncommon terms are specific to, or have Ada-specific terms.

# UNUSED, DO NOT SUBMIT

Ada is a honey badger that just doesn't care.

Ada focues on intent, usually describing the goal, rather than focusing on the method.