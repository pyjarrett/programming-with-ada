Language Summary of Key Points
==============================================================================

This tries to provide a reference for equivalent structures between Ada and
C++.  I'm filling in the chart a little at a time, `N_A` means that the
language has no equivalent.

Types used in Examples
------------------------------------------------------------------------------

|Concept|Ada|C++|Rust||
|-------|---|---|----|---|
|Ref|Access|Reference|Reference|Ada: Access only points to members in storage pool.|
|Ptr|Access All|Pointer|Pointer|Ada: Access all may point to a storage or non-storage pool address.|
|Namespace|My_Package|my_namespace|my_mod|Ada: Packages also act as elements of compilation units.|
|Namespace|P, Q, R|P, Q, R|P, Q, R|
|Class|Capricorn|Capricorn|Capricorn||
|Struct|Scorpio|Scorpio|Scorpio||
|Type|S, T, V, W|S, T, V, W|S, T, V, W|V and W, not U and V to be easier to read.|
|Variables|A, B, C|a, b, c|a, b, c||
|Function|Foo, Bar|foo, bar|foo, bar||

|Concept|Ada|C++|Rust||
|-------|---|---|----|---|
||||||
||||||
|**Overview**|||||
||||||
||||||
|Identifiers|Can't start with number or underscore, **case insensitive**`| `[a-zA-Z0-9_][a-zA-Z0-9_]*`||
|Keywords|Case insensitive, usually lower case|lower case|lower case|
|Naming Convention(s)|`Ada_Case`, `keywords`|`camelCase`, `PascalCase` (Unreal), `snake_case` (STL)|`snake_case` (functions), `PascalCase` (types)|
|Declaration file|`FileName.ads`|`FileName.h`|`FileName.rs`|Ada: `.ads` files are compiled, unlike headers.  Rust: No separate declaration/specification file is used. C++: Other possible extensions exist (`.hpp`, `.hh`, etc.)|
|Definition file|`FileName.adb`|`FileName.cpp`|`FileName.rs`|
|Dependency|`with Package.Child;`|`#include "FileName.h"`|`use my_mod;`|C++: Uses preprocessor.|
|||`#include <FileName.h>`||C++: Usage of <> vs "" is implementation-defined.
|Line comment|`-- line comment`|`// line comment`|`// line comment`||
|Block comment| _N/A_|`/*  */`|`/* /* nestable */ */`||
|Inline docs|`--` before or after element|`/** */` or `///` (doxygen)|`/** */`, `//!`||
||||||
||||||
|**Program Structure**|||||
||||||
||||||
|Compile-time configuration|_N/A_|`#if`|`#[cfg(...)]`||
|||`#ifndef`|||
|||`#ifdef`|||
|||`#define`|||
|Namespacing|`package P`|`namespace MyNamespace { }`|`mod module { }`||
|Child Namespaces|`package P.R`|`namespace P { namespace R {}}`|||
|||`namespace P::R { }` |||
|Namespace aliasing|`package TIO renames Ada.Text_IO;`|`namespace fs = std::filesystem;`|||
|Using namespace|`use Ada.Text_IO;`|`using namespace std;`||
|Scope resolution|`P.Q.R`|`P::Q::R`|`P::Q::R`||
||`private with Q`||||
|Ensuring stateless behavior|`package P with Pure`||Ada: Ensures that the package and everything it uses has no state.|
|No module initialization required|`pragma Preelaborate(P);`|||Ada: Ensures the package has no initialization routine.|
|Ensure elaboration immediately after specification|`pragma Elaborate_Body;`|||Prevents usage of components in a package before they are initialized.|
|Ensure other package is initialized before this one|`pragma Elaborate(P);`|||
|Ensure other package and all dependencies are initialized before this one|`pragma Elaborate_All(P);`|||
||`pragma Restrictions(No_Dependencies => Other_Package)`|||
||||||
||||||
|**Compile-time Computation**|||||
||||||
||||||
|Static assert|_N/A_|`static_assert(expr, "message");`|||
||_N/A_|`constexpr`|||
||||||
||||||
|**Accessing Memory**|||||
||||||
||||||
|Pointer to storage pool|`Ptr : access T;`|_N/A_||Ada: Accesses elements within storage pool, may not point to arbitrary locations.|
|Pointer|`Ptr : access all T;`|`T* ptr;`||Ada: May access storage pool or any `aliased` variable.|
|Pointer deference|`Ptr.all`|`*ptr`||Ada: Runtime null check might be done.|
|Reference|`Ptr : not null access T;`|`T& ptr;`|||
|Variable used by Pointer|`A: aliased T;`|_N/A_||Ada: Required to get an "access" to this value.|
|Address|`Ptr : access T := T'Access(A)`|`T* ptr = &A;`|||
|Address|`Ptr : access all T := T'Unchecked_Access(A)`| _N/A_|||
|Constant pointer|`Ptr : constant access T;`|`T* const ptr;`|||
|Pointer to constant|`Ptr : access constant T;`|`const T* ptr;`|||
|Constant pointer to constant|`Ptr : const access constant T`|`const T* const ptr;`|||
||`pragma Restrictions(No_Implicit_Heap_Allocation)`||Ada: Prevents even implicit heap allocations made by the compiler.|
||`pragma Restrictions(No_Anonymous_Allocators)`|||
||||||
||||||
|**Control Flow**|||||
||||||
||||||
|if|`if A then B; else C elsif D end if;`|`if (a) { B; } else { C; }`||Ada: Use `null;` statement if empty.|
|while|`while A loop B; end loop;`|`while (A) { B; }`||Ada: Use `null;` statement if empty.|
|do-while|_N/A_|`do { } while (A);`|||
|Value-based loop|`for Value in 0 .. 99`|`for (int i = 0; i < 100; ++i)`|||
|Iterator-based loop|`for Elem of Container`|`for (auto& elem : container)`|||
|Loop over enum|`for Elem in EnumName`|_N/A_|||
|Start next iteration|_N/A_|`continue`|||
||`exit`|`break`|||
|Start exception handling|_N/A_|`try`|_N/A_|Ada: Can put `exception` as ending section of any block of executable statements, such as in `package body` or `declare` or the end of a subprogram.|
|Exception handling|`exception`|`catch`|_N/A_||
||||||
||||||
|**Expressions**|||||
||||||
||||||
|If expression|`(if A then B else C)`|`A ? B : D`|||
|Qualified Expression|`for all A of B => A = 0`|_N/A_|||
||`for some A of B => A = 0`|_N/A_|||
||||||
||||||
|**Mathematics**|||||
||||||
||||||
|In-place math operations|_N/A_|`A += 1;`|||
|Pre-increment|_N/A_|`++a;`|||
|Post-increment|_N/A_|`a++;`|||
|Modulus|`mod`|`%`|||
|Remainder|`rem`|`std::div`|||
|Exponentiation|`a ** b`|_N/A_|||
|Bit shifting| _N/A_ | `A <<= B; A >>= B;`||C++: other ops exist, `<<`, `>>`.  Ada: Equivalent operations exist in the standard library.|
||||||
||||||
|**Boolean**||||
||||||
||||||
|Equality|`A = B;`|`A == B;`|`a == b`||
|Inequality|`A /= B;`|`A != B;`|`A != B;`|Ada:Inequality (`/=`) is automatically defined to be the opposite of equality if `=` is overriden to return a `Boolean`|
|not|`not`|`!`|||
|Boolean operations|`and`, `or`|`&`,`\|`||Ada: short versions are not shortcircuiting.  C++: Used rarely for optimizations to reduce branching since they make assumptions as to how boolean values are stored.|
|Short circuiting Boolean operations|`and then`, `or else`|`&&`,`\|\|`|`&&`,`\|\|`||
|xor|`xor`|`xor`|||
|Implies|`(if A then B)`|_N/A_||Ada: Equivalent to `(not A or B)`|
||||||
||||||
|**Functions and Procedures**||||
||||||
||||||
|Inline|`procedure Foo with Inline;`|`void Foo();`|`#[inline]`|
|Pass by pointer|`procedure Foo(in B: access Bar);`|`void foo(Bar* b);`||
|Pass by reference|`procedure Foo(in B: Bar); -- w/ limited type`|`void foo(Bar& b);`||Ada: limited types and tagged types are always passed by reference.|
|Similar to Haskell's `where`|`L2 : Integer renames V.Length * V.Length`||Renames a subexpression.|
|Using functions for a type as unqualified|`use type My_Package.Foo;`| _N/A_||Ada: Allows functions which use or return a type to be used without the package prefix.|
|const-correctness|`procedure Foo(in Bar);`|`void Foo(const Bar& bar);`|`fn foo(bar : &Bar)`|Ada: Treats parameters without `out` as constant.|
|Modifyable parameters|`procedure Foo(in out Bar);`|`void Foo(Bar& bar);`|`fn foo(bar : &mut Bar)`|
|Expression Function|`function Foo is (Some_Expression);`||Ada way to quickly write functions|
||||||
||||||
|**Types**||||
||||||
||||||
|Assignment|`A := B;`|`A = B;`|`a = b;`||Ada: Assignment is not an operator and cannot be overridden.|
|Multi-dimensional Array|`Mat4 : array (1 .. 4, 1 .. 4) of Float;`|`float Mat4[4][4];`|||
|Statically sized array|`type Buffer is array(1 .. 128) of Integer;`|`int Buffer[128];`||Ada: Convention seems to be indexes starting from 1, though arrays can be arbitrarily indexed.|
|Built-In Variable length array|`type Buffer is array(1 .. N) of Integer`| _N/A_||Ada variable-length arrays can avoid heap allocation and have their bounds determined at runtime, even when stored within types.  This behavior is still checked for size constraints.|
|Semantic type|`type Microseconds is new Integer;`| _N/A_|||
|Range checks|`type My_Positive is range 1 .. Integer;`| _N/A_ |||
|Type Aliasing|`subtype T is W;`|`using T = W;`|||
|||`typedef W T;`|||
|Sum Types|`type S is (T, V, W);`|`std::variant<T, V, W>`|`enum S { T, V, W }`||
|Inferred typing| _N/A_ | `auto x = /*expr*/`|||
|Coersion (casting)|`A := B'(C);`|`B a = static_cast<B>(c);`||
|Enum range|`A'Range`|_N/A_||Ada: Treated like a range, similar to `1 .. 3`, e.g. can be used like `for A in A'Range`|
|Size of a type|`A'Size`|`sizeof(A)`||
|Alignment|`A'Alignment`|`alignof(A)`||
|Membership test|`A in E`|||Ada: Works to see if types meet subtype contraints, also use to determine instance-of relationship.|
||||||
||||||
|**Object-Oriented Programming**||||
||||||
||||||
|Subprogram call (no parameters)|`A;`|`a();`|`a();`||
|Subprogram call of type|`A.B;`| `A.B();`|`a.b();`||
|Subprogram call of pointed to type|| `A->B();`|||
|Preventing copying|`type X is limited type;`|`class Foo { Foo(Foo&) = delete; Foo& operator=(const Foo&) = delete; };`|||
|Class-like|`type T is private;`|`class T {};`|`struct T {}`||
|Override specifier|`overriding`|`override`||
||`not overriding`|_N/A_||Ensure that a subprogram definition does not override an existing one.|
|Inheritance|`type Foo is Bar with null record`|`class Foo : public Bar {};`|
||`type Foo is Bar with record ... end record`|`class Foo : public Bar { ... };`|
|Passing parameter by base class|`BaseClass'Class`|`BaseClass&`|
|Empty statement|`null;`|`;`||
|||`do { } while (0)`;||
|Empty Procedure|`procedure Foo is null;`|`void Foo(){}`||
|||`explicit`||
|Dynamic allocation|`A : access T := new T;`|`T* a = new T()`|Most newer C++ code prefers the usage of `std::shared_ptr` or `std::unique_ptr`.|
|||`std::unique_ptr<T> a = std::make_unique<T>();`||
|||`std::shared_ptr<T> a = std::make_shared<T>();`||
|Dynamic dispatching|`procedure Foo(A : T'Class)`|`virtual void Foo();`||
|Type Parameterized by Value|_N/A_|`template <int T = 5>`||C++ is done at compile-time.|
|Type Parameterized by Value|`type S(T: t) is record -- ...`|_N/A_||Ada: Uses "discriminants" and is at runtime|
|Type Invariant|`type T is new V with Type_Invariant => Expr(T)`||||
|Runtime type checking|`A in T`|||
||`limited with P;`|||
||`private package My_Package`|||
||||||
||||||
|**String Handling**||||
||||||
||||||
|String concatenation|`A & B`|`std::string C = A + B;`|||



Terminology
==============================================================================

|Term|Ada|C++||
|----|----|----|----|
||tagged type|class||
||record|struct||
||parent||Non-abstract tagged type being extended.|
||pregenitor||Additional interfaces inherited|
|Heap|Storage pool|Heap|Ada: Storage pool isn't exactly the same as "heap" but is similar.|


Ada Verbiage
==============================================================================

- Limited Type - an uncopyable type
- ABE - "Access-before-elaboration"

