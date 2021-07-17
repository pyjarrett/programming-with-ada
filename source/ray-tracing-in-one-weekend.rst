Ray Tracing in One Weekend... in Ada
==============================================================================

I've been learning Ada 2012, it started out as an intellectual curiosity some
time in March, and then I bought
`John Barne's "Programming in Ada 2012" book <https://www.amazon.com/Programming-Ada-2012-John-Barnes/dp/110742481X>`_
for a more in-depth look.  A lot people don't really know hardly anything about
Ada, and I don't really blame them.  Until recently, resources were very scarce
and there's not good comparison programs to see what Ada is like.

I did a port of `Ray Tracing in One Weekend <https://raytracing.github.io/>`_
as `faithfully as possible from C++ into Ada <https://github.com/pyjarrett/ada-ray-tracer>`_.
This comparison should help people understand more of what Ada is about to
satisfy their own curiosity.

Overall, the port went very smoothly, due to the conceptual similarities
between C++ and Ada.  There's a few things to point out, and a few rough spots
with Ada which were frustrating.

Project Layout
------------------------------------------------------------------------------

This is the directory structure::

    ├── COPYING
    ├── README.md
    ├── obj
    ├── ray_tracer.gpr
    └── src
        ├── main.adb
        ├── rt-bmp.adb
        ├── rt-bmp.ads
        ├── rt-cameras.adb
        ├── rt-cameras.ads
        ├── rt-debug.adb
        ├── rt-debug.ads
        ├── rt-hitables.adb
        ├── rt-hitables.ads
        ├── rt-image_planes.adb
        ├── rt-image_planes.ads
        ├── rt-materials.adb
        ├── rt-materials.ads
        ├── rt-pseudorandom.adb
        ├── rt-pseudorandom.ads
        ├── rt-rays.adb
        ├── rt-rays.ads
        ├── rt-vecs.adb
        ├── rt-vecs.ads
        └── rt.ads

Many Ada projects use the GNAT ecosystem, which you can get from the Free
Software Foundation or with GNAT Community Edition, released by AdaCore.
`ray_tracer.gpr` is akin to a CMakeLists.txt, except for the GNAT Project
Manager `gprbuild <https://learn.adacore.com/courses/GNAT_Toolchain_Intro/chapters/gprbuild.html>`_
Most of the Ada ecosystem assumes that tools will get run from the command line,
and GPR files reference the appropriate options used by the related programs.
This file is an Ada-derived domain-specific language (DSL).

Projects might create multiple GPR files, and such as one for the main program, another
for building and running tests, or for example programs in libraries.

`main.adb` : Main Function
------------------------------------------------------------------------------

Files have three parts: a context clause, declarative parts, and executable parts.

.. code-block :: Ada

    -- context clause
    with GNATCOLL.Terminal;

    -- declarative part

    -- declare Main
    procedure Main is
        -- declarative part for elements used by Main
        use RT;
        Term_Info : GNATCOLL.Terminal.Terminal_Info;

    begin
        -- executable statements start here
        GNATCOLL.Terminal.Init_For_Stdout (Term_Info);

    exception
        -- exception handle for the executable block

        -- no empty blocks allowed, "null;" is the empty statement.
        null;
    end Main;

The context clause describes dependencies on library "packages", the declarative part
allows declaring and defining functions and variables, and the executable start is
linearly executable code.

Ada uses packages as proper modules instead of including files with a
preprocessor. Libraries are brought in using ``with``, with the dots in the name
describing the package path.  ``Terminal`` here is a child package of ``GNATCOLL``.

Unlike other languages, the entry procedure for an Ada program doesn't need to named ``main``.
Files defining program entries define your main function, but all functionality
must be brought in from libraries or in the declaration block of the main function.

Library "packages" follow a similar, but slightly different format than main files.
Packages get split between a public interface, described in a ``.ads`` file, while the package
body is in an ``.adb`` file.  This is similar to the header/source separation in
C or C++ libraries, except the compiler treats these as real entities.

Packages operate like namespaces, but also modules.  Packages can even include a ``begin``
section of initialization code to run before starting the main function, and describe
startup dependencies between packages.

Expression functions help knock down verboseness
------------------------------------------------------------------------------

Ada 2012 adds "expression functions" where instead of a full ``is begin ... end``
you can just wrap the expression describing the value to return in parentheses.
This made the vector implementation surprisingly terse.

.. code-block:: Ada

    function Reflect (V, N: Vec3) return Vec3 is (V - 2.0 * Dot (V, N) * N);

Semantic for types ("derived types") in Ada
------------------------------------------------------------------------------

The reference version uses type aliases for 3D vectors, which improves the look
of code but doesn't prevent misuses of types according to their semantics.

.. code-block:: C++

    using point3 = vec3;   // 3D point
    using color = vec3;    // RGB color

    vec3 v1, v2;
    point3 p1, p2;
    color c1, c2;

    c1 = p1;       // Allowed, but did you really mean this?
    p2 = p1 - c1;  // Subtract a color from a point?    

    refract (c1, v1, 0.5);  // Did you really mean to refract a color?

Yes, you can prevent such mistakes in C++ through inheritance or templates with
tags for types, but most people don't jump through these hoops.  You can specify
different semantic meanings for types in Ada with "derived types" which use the
same generated function code but the compilers prevents misuse in code.  These
aren't just type aliases, they're new actually new types

.. code-block:: Ada

    type Vec3 is record
        X, Y, Z : F32 := 0.0;
    end record;

    --------------------------------------------------------------------------
    -- Common operations for vector-like types.
    --------------------------------------------------------------------------

    function "+" (A, B : Vec3) return Vec3;
    function "-" (A, B : Vec3) return Vec3;
    function "*" (A, B : Vec3) return Vec3;

    -- SNIP!  A bunch more functions which relate to vectors, colors and points.

    type Color3 is new Vec3;
    type Point3 is new Vec3;

    --------------------------------------------------------------------------
    -- Define more specific functions to allow what we actually semantically to happen.
    --------------------------------------------------------------------------    

    function "+"(A : Point3; B : Vec3) return Point3 is (A + Point3(B));
    -- Translate a point by a vector.

    function "-"(To : Point3; From : Point3) return Vec3 is (Vec3(To) - Vec3(From));
    -- The vector giving a direction between two points.

    -- This is defined after we created Color3 and Point3, so Refract only
    -- works for Vec3.
    function Refract (UV, N : Vec3; Ni_Over_Nt : F32) return Vec3;

The ``Color3`` and ``Point3`` types get created with the same properties as ``Vec3``,
but with functions which only take their respective types.  Yes, you can force
the conversion but it will be explicit.

.. code-block:: Ada

    v1, v2 : Vec3;
    p1, p2 : Point3;
    c1, c2 : Color3

    c1 := p1;               // Error can't assign a color to a point.
    c1 := Color3(p1);       // Force the compiler to do this anyways.
    p2 := p1 - c1;          // Compile error, can't subtract a color from a point.
    p2 := v1 + v2;          // Compile error, a point is not a vector.
    p2 := Point3(v1 + v2);  // Force compiler to do this anyways.

Since ``Refract`` is defined after ``Color3`` and ``Point3`` were defined, it isn't
included as part of these types, so it can't be used.

.. code-block:: Ada

    Refract (c1, v1, 0.5);  // Compile error. Doesn't exist...

Ada lacks perfect forwarding
------------------------------------------------------------------------------

One of the best and killer features of modern C++ is `perfect forwarding <https://cpppatterns.com/patterns/perfect-forwarding.html>`_
combined with `parameter pack. <https://en.cppreference.com/w/cpp/language/parameter_pack>`_.
The gist of these features is that you can create your own functions which hand
off their arguments to another function as if that second function was called
directly.  This is especially useful in situations where you want to, for example,
imitate the interface of a constructor for a type, such as to construct an object
inside a container or as part of a smart pointer.

Let's say you have a type which looks like:

.. code-block:: C++

    class Widget {
    public:
        Widget(PermanentlyBound& foo, ExpensiveType&& e);
    };

You can actually call that constructor directly when making smart pointers in 
C++ (``unique_ptr`` or ``shared_ptr``):

.. code-block:: C++

    std::unique_ptr<Widget> widgetPtr = std::make_unique(someFoo, std::move(expensiveThing));

You see this lack of perfect forwarding and the copy required in ``RT.Materials``:

.. code-block:: Ada

    function Make_Material(Mat : Material'Class) return Material_Ptrs.Ref is
        Ptr : Material_Ptrs.Ref;
    begin
        -- Copy the value into the pointer.
        Ptr.Set (Mat);
        return Ptr;
    end Make_Material;

One line, 30% of runtime CPU
------------------------------------------------------------------------------

The most innocuous and expensive line of code in the program was the ``F32``
definition of the common floating point type used by the raytracer.  The issue
is the range check is defined to be the attribute range (read as "tick range").
This is really a little bit of error checking magic provided by Ada to find
infinities and NaN's whenever a ``F32`` is assigned to or used as a parameter,
which obviously happens a lot in a raytracer.  Disabling range checks or removing
the range for production use eliminates this problem, but it demonstrates how
much error checking is possible from a single line of code in Ada.

.. code-block:: Ada

    type F32 is new Interfaces.IEEE_Float_32 range Interfaces.IEEE_Float_32'Range;
