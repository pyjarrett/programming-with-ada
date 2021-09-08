Being More Terse
==============================================================================

Ada is known for being more long-winded than other languages because it prefers
keywords over symbology.  These techniques will help you compact your Ada text.

Expression Functions
--------------------

.. code-block:: ada                      
                                          
   function "-"(V : Vec3) return Vec3 is 
   begin                                 
        return (-V.X, -V.Y, -V.Z);        
   end "-";                              

Functions can return expressions without using a full ``begin`` ... ``end``
section.  Just wrap your expression in parentheses.

.. code-block:: ada                                         
                                                            
   function "-"(V : Vec3) return Vec3 is (-V.X, -V.Y, -V.Z);

Don't repeat parameter types
----------------------------

.. code-block:: ada                                
                                                   
   function Add (Left : Integer, Right : Integer)  
       return Integer;                             

If parameter types repeat, you can group them.

.. code-block:: ada                    
                                       
   function Add (Left, Right : Integer)
       return Integer;                 

Locally use packages
--------------------

.. code-block:: ada                                                               
                                                                                   
   procedure Test_Is_Quoted (Op : in out TT.Operation'Class) is                   
   begin                                                                          
       Op.Register;                                                               
       Op.Assert (not Is_Quoted (Ada.Characters.Latin_1.Quotation & "some text"));
   end Test_Is_Quoted;                                                            
                                                  
Using packages locally doesn't pollute your global namespace.                                

.. code-block:: ada                                              
                                                                 
   procedure Test_Is_Quoted (Op : in out TT.Operation'Class) is  
       use Ada.Characters.Latin_1;                               
   begin                                                         
       Op.Register;                                              
       Op.Assert (not Is_Quoted (Quotation & "some text"));      
   end Test_Is_Quoted; -- Visibility of Latin_1 ends here.       

Abbreviate conversions functions
--------------------------------

.. code-block:: ada                                                             
                                                                                
   Toggles.Append(Ada.Strings.Unbounded.To_Unbounded_String("--verbose"));      
   Toggles.Append(Ada.Strings.Unbounded.To_Unbounded_String("--skip-errors"));                                                                                
                                                                                
The usage of ``"+"`` as a function to convert from ``String`` to ``Ada.Strings.Unbounded.Unbounded_String`` is common:

.. code-block:: ada                                                                         
                                                                                            
   function "+"(S : String) return String renames Ada.Strings.Unbounded.To_Unbounded_String;
   Toggles.Append(+"--verbose");                                                            
   Toggles.Append(+"--skip-errors");                                                        

Locally define helper functions
-------------------------------

.. code-block :: ada

   procedure Test_Is_Quoted (Op : in out TT.Operation'Class) is
       use Ada.Characters.Latin_1;
   begin
       Op.Register;

       Op.Assert (not Is_Quoted(""));
       Op.Assert (not Is_Quoted ("not quoted"));
   end Test_Is_Quoted;

Using a locally defined helper function to simplify repeated local logic.  Note
that these local subprograms don't pollute the global namespace.

.. code-block :: ada

   procedure Test_Is_Quoted (Op : in out TT.Operation'Class) is
       use Ada.Characters.Latin_1;
       procedure Not_Quoted (S : String) is
       begin
           Op.Assert (not Is_Quoted(S));
       end Not_Quoted;
   begin
       Op.Register;
       Not_Quoted ("");
       Not_Quoted ("not quoted");        
   end Test_Is_Quoted;

Use package renames within ``package body``
-------------------------------------------

.. code-block :: ada

   with Ada.Text_IO;
   package body Hello_World is
       procedure Greet is
       begin
           Ada.Text_IO.New_Line;
           Ada.Text_IO.Put_Line ("Hello, world!");
           Ada.Text_IO.New_Line;
       end Greet;
   end Hello_World;

Show indication of where subprograms come from while shortening the names used
for them.  Names inside package bodies won't be visible.

.. code-block :: ada

   with Ada.Text_IO;
   package body Hello_World is
       package AIO renames Ada.Text_IO;

       procedure Greet is
       begin
           AIO.New_Line;
           AIO.Put_Line ("Hello, world!");
           AIO.New_Line;
       end Greet;
   end Hello_World;


Use a package at file scope, sparingly
--------------------------------------

.. code-block :: ada

   with Ada.Text_IO;
   package body Hello_World is
       package AIO renames Ada.Text_IO;

       procedure Greet is
       begin
           Ada.Text_IO.New_Line;
           Ada.Text_IO.Put_Line ("Hello, world!");
           Ada.Text_IO.New_Line;
       end Greet;
   end Hello_World;

Some packages provide well-recognizable subprograms and hence cannot be confused
easily.  When used often, using the package at the file scope can cut down
significantly on verboseness.

.. code-block :: ada

   with Ada.Text_IO;  use Ada.Text_IO;
   package body Hello_World is
       procedure Greet is
       begin
           New_Line;
           Put_Line ("Hello, world!");
           New_Line;
       end Greet;
   end Hello_World;

Provide abstraction without introducing more code
-------------------------------------------------

You might not know how you want to use a subprogram, but still want to separate it
from another one which could stand-in for it.

.. code-block :: ada

   package RT.Debug is
       procedure Print (Str : String) renames Ada.Text_IO.Put_Line;
   end RT.Debug;

Rename complicated expressions 
------------------------------

Sometimes you might have long complicated expressions, which you can rename,
which assigns their value when the renaming occurs.  This is not text 
substitution, so the initial value cannot be modified.

.. code :: ada

   with Ada.Text_IO;
   
   procedure Sample is
      use Ada.Text_IO;
   
      type Int_List is array (1 .. 10) of Integer;
      Values : Int_List := (others => 0);
      Index : Integer := 1;
      First : Integer renames Values(Index);
      Second : Integer renames Values(2);
   begin
      Put_Line (First'Image);
      Put_Line (Second'Image);
   
      New_Line;
      First := 5;
      Put_Line ("First changed to 5");
      Put_Line (First'Image);
      Put_Line (Second'Image);
   
      New_Line;
      Index := 2;
      Put_Line ("Index changed to 2");
      Put_Line (First'Image);
      Put_Line (Second'Image);
   
      New_Line;
      First := 7;
      Put_Line ("First changed to 7");
      Put_Line (First'Image);
      Put_Line (Second'Image);   
   end Sample;

Output:

.. code ::

    0
    0

    First changed to 5
    5
    0

    Index changed to 2
    5
    0

    First changed to 7
    7
    0