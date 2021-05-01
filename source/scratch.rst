

- Instead of providing values by `const &` and `&`, I would annotate those
  parameters as `in` and `in out` respectively.  The compiler decides how to
  pass those parameters, whether by copy or reference, with some types always
  being preferred to be passed by reference, such as uncopyable (Ada calls
  them `limited`) types.

- Indefinite type : a type for which you cannot declare an object without supply bounds a constraint or an initial value.

Packages provide both namespacing
as well as specifying the unit of compilation.  


Those familiar with C or C++ will be familiar with physical interface, in
which "private" details must be given in the public interface 

I won't describe it in detail here, but many of your normal tricks for reducing
compilation time via the structure of your header and source files apply.


+------------------------------------------------------------------------------+---------------------------------------------------------------------------------+
|                                                                              |                                                                                 |
|                                                                              |                                                                                 |
|                                                                              |                                                                                 |
|                                                                              |                                                                                 |
|                                                                              |                                                                                 |
|                                                                              |                                                                                 |
|                                                                              |                                                                                 |
|                                                                              |                                                                                 |
|                                                                              |                                                                                 |
|                                                                              |                                                                                 |
|                                                                              |                                                                                 |
|                                                                              |                                                                                 |
|                                                                              |                                                                                 |
|                                                                              |                                                                                 |
|                                                                              |                                                                                 |
|                                                                              |                                                                                 |
|                                                                              |                                                                                 |
|                                                                              |                                                                                 |
|                                                                              |                                                                                 |
|                                                                              |                                                                                 |
|                                                                              |                                                                                 |
|                                                                              |                                                                                 |
|                                                                              |                                                                                 |
+------------------------------------------------------------------------------+---------------------------------------------------------------------------------+




Types
=====

They're so powerful I can ignore them for now,
and then you can come back and reread this to understand that types can be
dropped into the system.

Types are important in Ada, so I'm going to do the most bizarre thing and
ignore them for now.  There are incredible number of rules, but the
crucial thing to understand is that most types can just be dropped into
the system interchangably.  Types are important, but the other structures
in which types are used are what makes Ada so powerful and expressive.

Ada turns the tables on types.  They do a lot of things within the language,
but the language isn't about types.  Types are consistent, there's not some
weird distinction between objects and "primitives".

Ada types do a lot more than "put the square peg in the square hole, the round
peg in the round hole" of some other languages.  They define sets of data and
are used like tokens for deciding which functions or procedures to call.



+--------------------------+------------------------------------------------------------+------------------------------------------------------------------+---------------------------------------------------+--------------------------------------------------------------+
|                          | .. code-block:: Ada                                        | .. code-block:: C++                                              |                                                   |                                                              |
|                          |                                                            |                                                                  |                                                   |                                                              |
|                          |                                                            |                                                                  |                                                   |                                                              |
|                          |                                                            |                                                                  |                                                   |                                                              |
+--------------------------+------------------------------------------------------------+------------------------------------------------------------------+---------------------------------------------------+--------------------------------------------------------------+
|                          | .. code-block:: Ada                                        | .. code-block:: C++                                              |                                                   |                                                              |
|                          |                                                            |                                                                  |                                                   |                                                              |
|                          |                                                            |                                                                  |                                                   |                                                              |
|                          |                                                            |                                                                  |                                                   |                                                              |
+--------------------------+------------------------------------------------------------+------------------------------------------------------------------+---------------------------------------------------+--------------------------------------------------------------+