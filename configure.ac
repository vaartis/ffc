AC_INIT(ffc, 0.1)

m4_include([m4/ocaml.m4])

AC_ARG_ENABLE([tests], AS_HELP_STRING([--enable-tests], [Enable tests]))

AC_PROG_CC

AC_PROG_OCAML

AC_CHECK_OCAML_PKG([llvm])
if test "$OCAML_PKG_llvm" = "no"; then
   AC_MSG_ERROR([OCaml 'llvm' module not found.])
fi

AS_IF([test "x$enable_tests" = "xyes"], [
  AC_CHECK_OCAML_PKG([oUnit])
  if test "$OCAML_PKG_oUnit" = "no"; then
     AC_MSG_ERROR([OCaml 'oUnit' module is required to run tests.])
  fi
])

AC_OUTPUT([Makefile])
