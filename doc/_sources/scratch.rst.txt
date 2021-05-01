

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
