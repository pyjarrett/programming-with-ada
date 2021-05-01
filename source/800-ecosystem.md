GNAT Community edition is the primary editor, but has some license limitations.

GNAT puts build infomration into a GNAT project file, which uses a syntax very
similar to Ada.

- Compiler
- Binder
- Linker

Example build command:

```bash
gprbuild -d -PD:\dev\ada\septum\septum.gpr -k -s -j12
```

Example reformat command:
```bash
gnat pretty -rnb -PD:\dev\ada\septum\septum.gpr D:\dev\ada\septum\src\sp-interactive.adb
```

Which autoformatter am I supposed to use with GNAT?

- [Style Guide](https://github.com/Componolit/ada-style)
- [Awesome Ada](https://github.com/ohenley/awesome-ada)



