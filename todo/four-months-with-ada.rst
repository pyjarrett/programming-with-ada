Four Months with Ada
====================

For the last four months I've been learning and writing Ada 2012.  This is a more
in-depth examination of the language.  I'm using the general programming
vernacular, rather than the Ada-specific definition of terms.

A Free-Function Focused C++, or a Strongly Typed C
--------------------------------------------------

Ada focuses on creating packages of code which contain types and related functions
which use those types.  Since it lacks implicit casts, it uses function overloading
as a key design element.

The language writes like a strongly typed C with a strong emphasis on defining
types and then functions for those types.  There's no preprocessor, so instead
of ``#include``, there are "packages" which contain related types and functions.
These also act as namespaces for functions instead of types themselves, so no
distinction exists between between how "free functions", "class functions", and
"member functions" are declared, they're all just functions within a package.

Focus on Intent
---------------

Ada source focuses on describing intent and modeling semantics.  For example,
function parameters can be either ``in``, ``out``, or both.  ``in`` parameters
are readonly, and while you can force passing by reference via specifics in
the language, you often just ignore how this happens.

Describing semantics goes all the way into primitive types, and the rules are
consistent between primitive and user-defined types.  Lightweight types with
domain-specific meaning can be created, and along with no implicit casting,
prevents mishandling of semantics on primitive types.
Interfaces rarely use ``Integer`` or ``Float`` directly, instead you'll find
semantic versions ("derived types") created such as "Miles" or "Kilometers".

.. code-block :: Ada

    type Kilometers is new Natural;
    type Miles is new Natural;

    K : Kilometers := 10;
    M : Miles := 50;

    M := K;  -- Compile error!

This seems bizarre but it plays into the extensive use of function overloading
in the language.  With implicit casting and with function overloading, creating
functions and transforms of types, even numerical is straightforward.
Compile and runtime checks provide bounds-checking and numerical types can have
their bounds constrained to "known good" values.

Enumeration types have first class support, with operations for ``'First`` and ``'Last``
of the set of values and ``'Pred`` (predecessor) and ``'Succ`` for individual values.
Enumeration over all values and conversions to and from strings and integers get
provided for free.

Ada adds built-in support for pre and post conditions, through the use of
language aspects.  This is a "killer feature" of Ada 2012, on top of all of the
other type checking and safety checking, pre and post conditions get provided
as part of the specification of the function.  Clients can see it as part of
the interface and the compiler inserts runtime checks if enabled.  A lot of
languages have an assertion mechanism which often effectively gets used for
these checks, but a built-in way of doing this which shows up in the interface
is a major game changer.  I found that adding pre and post condition checks
during debugging to be a very effective tool.  

.. code-block :: Ada

    function Merge (A, B : Context_Match) return Context_Match with
        Pre  => Is_Valid (A) and then Is_Valid (B),
        Post => Is_Valid (Merge'Result);

Types which expose no private state can also have type invariants which are
checked prior to usage as function arguments and after assignments.

.. code-block :: Ada

   type Spinner is record
       Ticks_Per_Move : Positive;
       Ticks          : Natural;
       State          : Spinner_State;
       Style          : Spinner_Style;
   end record with
       Type_Invariant => Ticks < Ticks_Per_Move;

Another strange concept which I hadn't seen before, is that types
can be "parameterized" at runtime using what is called a "discriminant."
It's a sort of read-only constant field which is part of the record.

This read only value becomes part of the type and whether explicitly
described or not affects type checking.  An extremely commonly used
one is with the built-in String type in which the string's length
is given by the discriminant.  Ada already doesn't






# UNUSED, DO NOT SUBMIT

Ada is a honey badger that just doesn't care.

Ada focues on intent, usually describing the goal, rather than focusing on the method.