dnl
dnl Execute arbitrary emacs lisp
dnl
AC_DEFUN(AC_EMACS_LISP, [
elisp="$2"
if test -z "$3"; then
	AC_MSG_CHECKING(for $1)
fi
AC_CACHE_VAL(EMACS_cv_SYS_$1,[
	EMACS_cv_SYS_$1=`${EMACS} -batch -q -no-site-file -eval "(let ((x ${elisp})) (if (stringp x) (princ x) (prin1-to-string x)) (kill-emacs))"`
])
$1=${EMACS_cv_SYS_$1}
if test -z "$3"; then
	AC_MSG_RESULT($$1)
fi
])

AC_DEFUN(AC_XEMACS_P, [
  AC_MSG_CHECKING([if $EMACS is really XEmacs])
  AC_EMACS_LISP(xemacsp,(if (string-match \"XEmacs\" emacs-version) \"yes\" \"no\") ,"noecho")
  XEMACS=${EMACS_cv_SYS_xemacsp}
  EMACS_FLAVOR=emacs
  if test "$XEMACS" = "yes"; then
     EMACS_FLAVOR=xemacs
  fi
  AC_MSG_RESULT($XEMACS)
  AC_SUBST(XEMACS)
  AC_SUBST(EMACS_FLAVOR)
])

dnl
dnl Determine the emacs version we are running.
dnl Automatically substitutes @EMACS_VERSION@ with this number.
dnl
AC_DEFUN(AC_EMACS_VERSION, [
AC_MSG_CHECKING(for emacs version)
AC_EMACS_LISP(version,(and (boundp 'emacs-major-version) (format \"%d.%d\" emacs-major-version emacs-minor-version)),"noecho")
EMACS_VERSION=${EMACS_cv_SYS_version}
AC_MSG_RESULT(${EMACS_VERSION})
AC_MSG_CHECKING(for emacs type)
type=`$EMACS -q -no-site-file -batch -l $srcdir/lisp/guess -f guess`
case $type in
nemacs)
	EL_FILES="tc-is18.el"
	ELC_FILES="tc-is18.elc"
	NO_COMPILE_EL_FILES="tc-sysdep.el tc-is19.el tc-is20.el tc-is22.el"
	;;
mule1)
	EL_FILES="tc-is18.el"
	ELC_FILES="tc-is18.elc"
	NO_COMPILE_EL_FILES="tc-sysdep.el tc-is19.el tc-is20.el tc-is22.el"
	;;
mule4|mule5)
	EL_FILES="tc-is20.el"
	ELC_FILES="tc-is20.elc"
	NO_COMPILE_EL_FILES="tc-sysdep.el tc-is18.el tc-is19.el tc-is22.el"
	;;
mule6)
	EL_FILES="tc-is22.el"
	ELC_FILES="tc-is22.elc"
	NO_COMPILE_EL_FILES="tc-sysdep.el tc-is18.el tc-is19.el tc-is20.el"
	;;
xemacs21)
	EL_FILES="tc-is19.el"
	ELC_FILES="tc-is19.elc"
	NO_COMPILE_EL_FILES="tc-sysdep.el tc-is18.el tc-is20.el tc-is22.el"
	;;
mule*|xemacs20)
	EL_FILES="tc-is19.el"
	ELC_FILES="tc-is19.elc"
	NO_COMPILE_EL_FILES="tc-sysdep.el tc-is18.el tc-is20.el tc-is22.el"
	;;
*)
	echo "Unsupported Editor ($type)!!"
	exit 1
esac
AC_MSG_RESULT($type)
AC_SUBST(EMACS_VERSION)
AC_SUBST(EL_FILES)
AC_SUBST(ELC_FILES)
AC_SUBST(NO_COMPILE_EL_FILES)
])

dnl
dnl Determine whether the specified version of Emacs supports packages
dnl or not.  Currently, only XEmacs 20.3 does, but this is a general
dnl check.
dnl
AC_DEFUN(AC_EMACS_PACKAGES, [
AC_ARG_WITH(package-dir,
changequote(<<, >>)dnl
  --with-package-dir      Configure as a XEmacs package in directory,
changequote([, ])dnl
 [ EMACS_PACKAGE_DIR="${withval}"])
if test -n "$EMACS_PACKAGE_DIR"; then
  if test "$prefix" != "NONE"; then
	AC_MSG_ERROR([--with-package-dir and --prefix are mutually exclusive])
  fi
  dnl Massage everything to use $(prefix) correctly.
  prefix=$EMACS_PACKAGE_DIR
  datadir='$(prefix)/etc/tc2'
  infodir='$(prefix)/info'
dnl  lispdir='$(prefix)/lisp/tc2'
fi
AC_SUBST(EMACS_PACKAGE_DIR)
])
