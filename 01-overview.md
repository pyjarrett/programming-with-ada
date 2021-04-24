Article Goals
==============================================================================

This series intends to be multiple parts for different audiences:
- A Brief Overview Introducing Ada
- Comparison of Ada Constructs to C++
- Suggestions to improve the adoption of Ada. (for Adacore)

While my experience with Ada has been positive, this article aims to be as 
fact-driven and neutral as possible, attempting to give an objective
overview of the language for those unfamiliar with it, as well as 
describing the tradeoffs of using the language.

I am a Windows/Linux developer who primarily works in C++, but
has worked in many other languages in descending level of familiarity: Python,
C, Objective-C, Ruby, Rust, Haskell, Clojure, Perl, some Javascript, and PHP.
I stopped writing Rust consistently right around the time that async/await was
stabilized.  Time permitting I will attempt to write more detailed comparisons
against Rust as well.

I started reading about Ada because a developer I worked with several years 
ago had mentioned it, and I couldn't find a single person who had worked in it.
My intention at the time had been to try it out over the weekend, and after
finding a horribly broken bloated and bureaucratic language, never use it again.
I've now been learning and writing Ada 2012 for about a month.

High-level Language Features
==============================================================================

Ada supports:
- Forced namespacing.
- Dynamic binding (late-binding, "virtual" functions).
- Compiler and runtime checked type constraints.
- Runtime type invariant checking.
- Semantic types (e.g. saying two things are the same backing type, but not the
  same type of "thing", think of "Miles" vs "Kilometers" both stored as floats,
  which cannot be assigned to each other).
- RAII (controlled types).
- Design-by-contract (inline preconditions and post-conditions).
- Monitor types (protected types).
- Task definition with defined synchronization.
- Formal verification, by enabling `Spark_Mode` for parts of the program and
  writing in SPARK, a language which is an Ada subset.  Think of this along
  the lines of using `extern "C"`.

Ada is missing:
- A preprocessor (GNAT has one, but it's not standard).
- A sanitary macro system.
- Reflection.
- A concept of "move".

The Conceptual Framework of Ada
==============================================================================

Ada descends from Pascal, and yet uses many concepts already familiar to C or
C++ programmers.  As a long time C++ programmer, I find Ada builds on my
existing mental models while expressing my intent, with the compiler figuring
out than details.

> Based on its feature set, I'd describe Ada as a Pascal family mirror of C++,
> with many features of Rust.

Packages
------------------------------------------------------------------------------

As the fundamental building block of Ada, "packages" compose Ada programs,
namespaces which exhibit the logical and physical interfaces in a manner
similar to C++ header and source files.  Packages provide both namespacing
as well as specifying the unit of compilation.  Package specifications, usually
appear in `*.ads` (Ada specification) files, with their implementations
("bodies") in `*.adb` (Ada body) files.

Those familiar with C or C++ will be familiar with physical interface, in
which "private" details must be given in the public interface such as to ensure
size details for structs and classes are provided when needed.

Ada packages can also be nested and support visiblity rules for sharing details
with child packages.

Packages can also contain initialization code for the package to run at program
startup prior to entering the main procedure, with the ability to describe
dependencies in package start up order. This solves a specific C++ issue in
which static initialization order is not known, while also avoiding deferred
first-time usage costs, such as with singletons.

```ada
------------------------------------------------------------------------------
-- Sample.ads
--
-- Package specification
package Sample is
    -- interface

    -- Analogous to a struct.
    type Scorpio is record;
        Sample : Integer;
    end record;

    -- "Class declaration"
    type Capricorn is private;

private
    -- physical interface

    -- "Class definition"
    type Capricorn is record
        Age : Integer;
    end record;
end Sample;

------------------------------------------------------------------------------
-- in Sample.adb
--
-- Package body
package body Sample is
    -- implementation details

    -- Function used only in implementation
    procedure Foo is
    begin
        null;
    end Foo;

begin -- (optional)
    -- Initialization code to run at startup (optional)
end Sample;
```

```C++
//////////////////////////////////////////////////////////////////////////////
// MyFile.h
#pragma once

// Part of the physical interface, since compilers need to know the size of the
// struct when passed by value or used on the stack.
struct Scorpio {
    int sample;
};

// Public interface
class Capricorn {
public:
    Capricorn();

private:
    // Physical interface, visible due to technical limitations of the
    // compilation model.
    int age;
};

//////////////////////////////////////////////////////////////////////////////
// MyFile.cpp
// Implementation details.

namespace {
// Function used only in implementation.
void foo() {}
} // namespace

Capricorn::Capricorn {}
```

The Core Tenet of Ada
------------------------------------------------------------------------------

Program text should be clear without having to read ahead, referred to as:
**"Linear elaboration of declarations"**.  A clear demarcation exists between
declarations of things to exist and executable statements which use those things.

The simplicity backing this is the ability to make any declaration, including
types, variables and functions/procedures in any declaration block.  This means
the basic rule of "declare, then use" repeats itself throughout the language,
in `package/package body`, `task/task body`, declarations
used for subprograms (functions/procedures), and explicitly within statements
to execute with a `declare ... begin ... end` block.

Ada does not provide separate syntactical units for classes, structs and
namespaces.  Instead, packages contain types, constants and related subprograms
(functions and procedures).  A lot of specialized syntax goes away due to
this, for example there are no "member functions" and "class functions" and
hence no specialized syntax for things like member function pointers or class
function pointers exist.

Namespacing and overloading on parameters and/or the returned type determine
the subprogram called.

What would be "member functions" in C++ have the "controlling type(s)" as the
first parameter(s).  "`const` member functions" pass in the type as an `in` parameter,
which are immutable.  "non-`const` member functions" pass in the type as an
`in out` parameter, allowing the parameter to be modified.  This mirrors
Rust's notation wherein it reflects C++-like `const` behavior of member
functions with `self`, `&self`, and `&mut self` as a first parameter.
These are referred to as "primitive operations."

```ada
package body Quadratic is
    type Equation is record
        A, B, C: Float;
    end record;    

    type Root is new Float range Float'Range;
    -- Root type to catch generation of NaN and Inf.
    
    function Image(E : Equation) return String is
    begin
        return "" & Float'Image(E.A) & " x^2 + "
          & Float'Image(E.B) & " x + "
          & Float'Image(E.C);
    end Image;
    
    Epsilon : constant := 0.001;
    function Is_Linear(E : Equation) return Boolean is (abs (E.A) < Epsilon);    
    function Discriminant(E : Equation) return Float is (E.B ** 2 - 4.0 * E.A * E.C);

    function Sqrt(F : Float) return Float renames Ada.Numerics.Elementary_Functions.Sqrt;
    function Positive_Root(E: Equation) return Root is (Root((-E.B + Sqrt(Discriminant(E))) / (2.0 * E.A)));
    function Negative_Root(E: Equation) return Root is (Root((-E.B - Sqrt(Discriminant(E))) / (2.0 * E.A)));
    
    procedure Print_Solutions(E : Equation) is
        D : Float := Discriminant(E);
    begin
        if D < 0.0 then
            -- No roots.
            Ada.Text_IO.Put_Line("No real roots");
        elsif Is_Linear(E) then
            Ada.Text_IO.Put_Line(Root'Image(Root(-E.C / E.B)));
        else
            -- 2 roots.
            Ada.Text_IO.Put_Line(Root'Image(Positive_Root(E)));
            Ada.Text_IO.Put_Line(Root'Image(Negative_Root(E)));
        end if;        
    end Print_Solutions;
end Quadratic;
```

RAII
==============================================================================

Ada supports RAII by extending the `Controlled` type.

```ada
with Ada.Finalization;  use Ada.Finalization;
package Sample is
    -- "Controlled" types exhibit RAII behavior:
    type Capricorn is new Controlled with
    record
        Dummy : Integer;
    end record;

    overriding procedure Initialize(C : in out Capricorn);
        -- Initialization after creation.

    overriding procedure Adjust(C : in out Capricorn);    
        -- Adjustment after assignment.

    overriding procedure Finalize(C : in out Capricorn);
        -- Different than Java's Finalize, in that it's deterministic and more
        -- analogous to a C++ destructor.
        
    -- If you don't want one of these do to anything, you can avoid writing a
    -- definition in the package body and define the function as "do nothing"
    -- by writing:
    --
    -- overriding procedure Finalize(C : in out Capricorn) is null;
end Sample;

package body Sample is
    procedure Initialize(C : in out Capricorn) is
    begin
        -- Do something on initialize.
    end Initialize;

    procedure Adjust(C : in out Capricorn) is
    begin
        -- Adjustment after assignment.
        -- 
        -- If you want Adjust to do the same as Initialize and use the same object
        -- code without generating a separate function, you can just do
        -- procedure Adjust(C: in out Capricorn) renames Initialize;
    end Adjust;

    overriding procedure Finalize(C : in out Capricorn);
        -- Different than Java's Finalize, in that it's deterministic and more
        -- analogous to a C++ destructor.
end Sample;
```

```C++
class Capricorn {
public:
    // Similar for all constructors.
    Capricorn () {}

    // Copy constructor.
    Capricorn(const Capricorn&) {}

    // Move constructor.
    Capricorn(Capricorn&&) {}

    // Copy assignment.
    Capricorn& operator=(const Capricorn&) { return *this; }

    // Move assignment.
    Capricorn&& operator=(Capricorn&&) { return *this; }

    // Destructor.
    ~Capricorn () {}
};
```
