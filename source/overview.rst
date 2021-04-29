Article Goals
==============================================================================

This series intends to be multiple parts for different audiences:
- A Brief Overview Introducing Ada
- [Table comparison of Ada Constructs to C++](02-comparison.md)

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

> Based on its feature set, I'd describe Ada as a mirror of C++ in the Pascal family,
> with many features of Rust.

Ada supports:
- Forced namespacing.
- Sum types (discriminants).
- Parameterized types.
- Static polymorphism (generics).
- Dynamic polymorphism (dynamic dispatch, "virtual" functions).
- Compiler and runtime checked constaints on ranges of numerical types.
- Runtime type invariant checking on assignment and usage as parameters.
- Semantic types, saying two things are the same backing type, but not the
  same type of "thing", think of "Miles" vs "Kilometers" both stored as floats,
  which cannot be assigned to each other.
- RAII, deterministic construction and destruction of objects (controlled types).
- Design-by-contract (inline preconditions and post-conditions).
- Lifetime checks ("accessibility", but not as extensive as Rust).
- Monitor types (protected types).
- Task definition with defined synchronization.
- Formal verification, by enabling `Spark_Mode` for parts of the program and
  writing in SPARK, a language which is an Ada subset.  Think of this along
  the lines of using `extern "C"`, except for "provable" parts of your code base.
- Exceptions.
- Deterministic static initialization order.

Ada is missing:
- A preprocessor (GNAT has one, but it's not standard).
- A sanitary macro system.
- Reflection.
- A concept of "move".
- Variadic functions.
- Variadic templates.

The Conceptual Framework of Ada
==============================================================================

Ada descends from Pascal, and yet uses many concepts already familiar to C or
C++ programmers.  As a long time C++ programmer, I find Ada leverages the concepts
I'm used to in more formal, and often compiler-check ways.

For example:
- Instead of an header file providing an informal spec and an associated source
  file being the translation unit, in Ada a package is roughly analogous to a
  "header file," and that package's body is the "source file".
- Instead of providing values by `const &` and `&`, I would annotate those
  parameters as `in` and `in out` respectively.  The compiler decides how to
  pass those parameters, whether by copy or reference, with some types always
  being preferred to be passed by reference, such as uncopyable (Ada calls
  them `limited`) types.

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
which static initialization order is not known, while also offering the ability
to avoid deferred first-time usage costs, such as with singletons.

```ada
------------------------------------------------------------------------------
-- Example.ads
--
-- Package specification
package Example is
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
end Example;

------------------------------------------------------------------------------
-- in Example.adb
--
-- Package body
package body Example is
    -- implementation details

    -- Function used only in implementation
    procedure Foo is
    begin
        null;
    end Foo;

begin -- (optional)
    -- Initialization code to run at startup (optional)
end Example;
```

```C++
//////////////////////////////////////////////////////////////////////////////
// Sample.h
#pragma once
namespace Example {

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

} // namespace Example

//////////////////////////////////////////////////////////////////////////////
// Sample.cpp
// Implementation details.
namespace Example {

namespace {
// Function used only in implementation.
void foo() {}
} // namespace

Capricorn::Capricorn {}

} // namespace Example
```

The Core Tenet of Ada
------------------------------------------------------------------------------

Program text should be clear without having to read ahead, referred to as:
**"Linear elaboration of declarations"**.  A clear demarcation exists between
declarations of things to exist and executable statements which use those things.

The simplicity backing this is the ability to make any declaration, including
types, variables, functions/procedures, and packages in any declaration block.  This means
the basic rule of "declare, then use" repeats itself throughout the language,
in `package/package body`, `task/task body`, declarations
used in subprograms (functions/procedures), and explicitly within statements
to execute with a `declare ... begin ... end` block.

```ada
package body P is
    -- Declarations for the body of P go here.

    procedure Foo is
        -- Declarations for Foo can go here.

        -- Declare a function, only visible to Foo, to be used to implement Foo.
        procedure Bar is
        begin
            null; -- "null statement" here since this function actually does nothing...
        end Bar;
    begin
        -- Executable statements go here.
        Ada.Text_IO.Put_Line("Hello, World!");
        
        -- Declare a package here, inside the function, to show that you can.
		declare
			package Wat is
				type Capricorn is new Float;
			end Wat;
            -- A variable created using the package defined inside of Foo.
			Temp: constant Wat.Capricorn := 0.0;
		begin
            -- Print 0.0 as well.
			Ada.Text_IO.Put_Line(Wat.Capricorn'Image(Temp));
		end;
    end Foo;
begin
    -- Static initialization body of P.
end P;
```

Ada does not provide separate syntactical units for classes, structs and
namespaces.  Instead, packages contain types, constants and related subprograms
(functions and procedures).  A lot of specialized syntax goes away due to
this, for example there are no "member functions" and "class functions" and
hence no specialized syntax for things like member function pointers or class
function pointers exist.  Namespacing and overloading on parameters and/or the returned type determine
the subprogram called.

What would be "member functions" in C++ have the "controlling type(s)" as the
first parameter(s).  "`const` member functions" pass in the type as an `in` parameter,
which are immutable.  "non-`const` member functions" pass in the type as an
`in out` parameter, allowing the parameter to be modified.  This mirrors
Rust's notation wherein it reflects C++-like `const` behavior of member
functions with `self`, `&self`, and `&mut self` as a first parameter.
These are referred to as "primitive operations."

```ada
    -- Box "non-const function"
	procedure Move(Box : in out AABB2; Direction : Float2) is
	begin
		Box.Min := Box.Min + Direction;
		Box.Max := Box.Max + Direction;
	end Scale;
```

```ada
    -- Box "const function"
	function Midpoint(Box : in AABB2) return Float is
	begin
		return (Box.Min + Box.Max) / 2.0;
	end Scale;
```

RAII
==============================================================================

Ada supports [RAII](https://en.wikipedia.org/wiki/Resource_acquisition_is_initialization)
by extending the `Controlled` type.

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
