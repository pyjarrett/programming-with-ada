Advanced Techniques
===================

RAII
------------------------------------------------------------------------------

Ada supports `RAII <https://en.wikipedia.org/wiki/Resource_acquisition_is_initialization>`_
by extending the `Controlled` type.

+-------------------------------------------------------------------------------------------+--------------------------------------------------------------+
| .. code-block:: ada                                                                       | .. code-block:: c++                                          |
|                                                                                           |                                                              | 
|     with Ada.Finalization;  use Ada.Finalization;                                         |    class Capricorn {                                         | 
|     package Sample is                                                                     |    public:                                                   | 
|         -- "Controlled" types exhibit RAII behavior:                                      |    // Similar for all constructors.                          | 
|         type Capricorn is new Controlled with                                             |    Capricorn () {}                                           | 
|         record                                                                            |                                                              | 
|             Dummy : Integer;                                                              |    // Copy constructor.                                      | 
|         end record;                                                                       |    Capricorn(const Capricorn&) {}                            | 
|                                                                                           |                                                              | 
|         overriding procedure Initialize(C : in out Capricorn);                            |    // Move constructor.                                      | 
|             -- Initialization after creation.                                             |    Capricorn(Capricorn&&) {}                                 | 
|                                                                                           |                                                              | 
|         overriding procedure Adjust(C : in out Capricorn);                                |    // Copy assignment.                                       | 
|             -- Adjustment after assignment.                                               |    Capricorn& operator=(const Capricorn&) { return *this; }  | 
|                                                                                           |                                                              | 
|         overriding procedure Finalize(C : in out Capricorn);                              |    // Move assignment.                                       | 
|             -- Different than Java's Finalize, in that it's deterministic and more        |    Capricorn& operator=(Capricorn&&) { return *this; }       |
|             -- analogous to a C++ destructor.                                             |                                                              | 
|                                                                                           |    // Destructor.                                            | 
|         -- If you don't want one of these do to anything, you can avoid writing a         |    ~Capricorn () {}                                          | 
|         -- definition in the package body and define the function as "do nothing"         |    };                                                        | 
|         -- by writing:                                                                    |                                                              | 
|         --                                                                                |                                                              | 
|         -- overriding procedure Finalize(C : in out Capricorn) is null;                   |                                                              | 
|     end Sample;                                                                           |                                                              | 
|                                                                                           |                                                              | 
|     package body Sample is                                                                |                                                              | 
|         procedure Initialize(C : in out Capricorn) is                                     |                                                              | 
|         begin                                                                             |                                                              | 
|             -- Do something on initialize.                                                |                                                              | 
|         end Initialize;                                                                   |                                                              | 
|                                                                                           |                                                              | 
|         procedure Adjust(C : in out Capricorn) is                                         |                                                              | 
|         begin                                                                             |                                                              | 
|             -- Adjustment after assignment.                                               |                                                              | 
|             --                                                                            |                                                              | 
|             -- If you want Adjust to do the same as Initialize and use the same object    |                                                              | 
|             -- code without generating a separate function, you can just do               |                                                              | 
|             -- procedure Adjust(C: in out Capricorn) renames Initialize;                  |                                                              | 
|         end Adjust;                                                                       |                                                              | 
|                                                                                           |                                                              | 
|         overriding procedure Finalize(C : in out Capricorn);                              |                                                              | 
|             -- Different than Java's Finalize, in that it's deterministic and more        |                                                              | 
|             -- analogous to a C++ destructor.                                             |                                                              | 
|     end Sample;                                                                           |                                                              | 
+-------------------------------------------------------------------------------------------+--------------------------------------------------------------+ 


Timing out on a Blocking Operation
------------------------------------------------------------------------------

Sometimes you want to continue if an operation blocks, or continue after a timeout

+-------------------------------------------------------------------------------------------+
| .. code-block:: ada                                                                       |
|                                                                                           |
|    task body My_Task is                                                                   |
|        Elem : A_Queue_Element;                                                            |
|    begin                                                                                  |
|        loop -- processing loop                                                            |
|            select                                                                         |
|                A_Queue.Blocking_Queue (Elem);                                             |
|            or                                                                             |
|                -- Stop processing after a 1 second timeout. Removing this delay causes    |
|                -- immediate exit if a block occurs.                                       |
|                delay 1.0;                                                                 |
|                exit;                                                                      |
|            end select;                                                                    |
|                                                                                           |
|            -- ... process Elem ...                                                        |
|                                                                                           |
|        end loop                                                                           |
|    end My_Task;                                                                           |
+-------------------------------------------------------------------------------------------+


Waiting for all Tasks to Complete
------------------------------------------------------------------------------

A list of statements doesn't exit until all tasks are complete, so by using
`declare ... begin ... end` you can wait until all your tasks are done.

+-------------------------------------------------------------------------------------------+
| .. code-block:: ada                                                                       |
|                                                                                           |
|    declare                                                                                |
|        A_Task : My_Task;  -- task which needs to finish before more processing            |
|    begin                                                                                  |
|        null; -- Just wait until the task is done.                                         |
|    end;                                                                                   |
|                                                                                           |
|    -- Continue other operations here.                                                     |
+-------------------------------------------------------------------------------------------+
