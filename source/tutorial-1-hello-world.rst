Tutorial 1: Hello world with Alire
==================================

    All tutorials are considered "Work in Progress".

Most Ada tutorials don't go over environment setup.  This one will.  I'll be
using the Free Software Foundation's GNAT.

For things to install, you should grab:

- `Visual Studio Code <https://code.visualstudio.com/Download>`_ , a text editor
- `Ada Visual Studio Code Extension <https://marketplace.visualstudio.com/items?itemName=AdaCore.ada>`_,
  to make Visual Studio Code recognize Ada.
- `Alire <https://alire.ada.dev/>`_, an Ada helper tool and package manager.
  This is how we'll be installing our toolchain.

Once everything is installed you need to ensure that your OS can find these things in ``$PATH.``
On Windows it might look like:
- ``C:\GNAT\2021\bin``
- ``C:\Program Files\Alire\bin\``

Alire (pronounced "a lyre", like the instrument), is a freshly available tool for
managing dependencies in Ada and building and running your programs.

We want to start a new project, which we're going to call ``hello_world``.  It's
going to be a program we can run, so it needs to be a "binary."

.. code-block:: powershell

   alr init --bin hello_world

This creates a new folder ``hello_world/`` with your project in it.  It's
structure looks something like this:

.. code-block::

   hello_world/             -- what we named the project
   │   .gitignore           -- a gitignore to skip Ada files
   │   alire.lock           -- stores dependency info
   │   alire.toml           -- the "manifest file", info about dependencies
   │   hello_world.gpr      -- project file with build configuration
   │
   ├───alire                -- ignore this, it's alire-specific
   ├───bin                  -- where programs get put when built
   ├───obj                  -- ignore this, it's a compiler scratch pad
   └───src                  -- source code, more files go here
        hello_world.adb     -- source file containing our program start

This setup is the convention used by Alire, and often by other projects, but
isn't required.  A full discussion of GPR files are outside the scope of this
article, if you open up ``hello_world.gpr``, you can see how these directories
are configured:

.. code-block:: Ada

   for Source_Dirs use ("src");
   for Object_Dir use "obj";
   for Exec_Dir use "bin";   
   for Main use ("hello_world.adb");

This is more for your edification, you don't really need to bother with GPR
files for a long while.

It should look something like this in ``hello_world.adb``:

.. code-block:: Ada

    procedure Hello_World is
    begin
       null;
    end Hello_World;

Ada is a little strange from other languages in that the program doesn't have to
start from a point named "main", and you can name that entry point however you
like.  In our case, it's just called ``Hello_World``.

You'll see this ``begin`` .. ``end`` pattern repeated a lot since it's used to
describe "these are steps the program should do."  A list of statements cannot
be empty, so ``null;`` indicates there's no work to be done.

.. code-block:: Ada   

    procedure Hello_World is  -- tell what the structure is
    begin                     -- start executing
       null;
    end Hello_World;          -- we're done executing

This program doesn't do anything and that ``null`` looks a bit weird.

Let's break this down:

.. code-block:: Ada

   with Ada.Text_IO;
   
   procedure Hello_World is
   begin
     Ada.Text_IO.Put_Line("Hello, world!");
   end Hello_World;


.. code-block:: powershell

   PS D:\dev\ada\hello_world> alr build
   Setup
      [mkdir]        object directory for project Hello_World
      [mkdir]        exec directory for project Hello_World
   Compile
      [Ada]          hello_world.adb
   Bind
      [gprbind]      hello_world.bexch
      [Ada]          hello_world.ali
   Link
      [link]         hello_world.adb


.. code-block:: powershell

   alr run


.. code-block:: Ada

   for Main use ("hello_world.adb");


.. code-block:: powershell

   Compile
      [Ada]          hello_world.adb
   hello_world.adb:5:03: (style) bad indentation
   
      compilation of hello_world.adb failed


You can actually just run `alr run`, you don't actually need to run `alr build`
separately.

.. code-block:: powershell

   PS D:\dev\ada\hello_world> alr run
   Compile
      [Ada]          hello_world.adb
   Bind
      [gprbind]      hello_world.bexch
      [Ada]          hello_world.ali
   Link
      [link]         hello_world.adb
   Hello, world!

When your program doesn't need to build, the output will be somewhat different.

.. code-block:: powershell

   PS D:\dev\ada\hello_world> alr run
   gprbuild: "hello_world.exe" up to date
   Hello, world!

.. code-block:: Ada

   with Ada.Text_IO;  use Ada.Text_IO;
   
   procedure Hello_World is
   begin
     Put_Line("Hello, world!");
   end Hello_World;

You can also shorten names as well.

.. code-block:: Ada

   with Ada.Text_IO;  use Ada.Text_IO;
   
   procedure Hello_World is
     package AIO renames Ada.Text_IO;
   begin
     AIO.Put_Line("Hello, world!");
   end Hello_World;

