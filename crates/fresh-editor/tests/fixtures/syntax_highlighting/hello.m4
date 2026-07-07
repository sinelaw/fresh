dnl Configure feature detection.
AC_INIT([fresh-demo], [1.0])
AC_CONFIG_SRCDIR([src/main.c])

define(`fresh_message', `Fresh $1')
ifdef(`ENABLE_DEBUG', [
  AC_DEFINE([DEBUG], [1], [Enable debug logging])
], [
  AC_MSG_NOTICE([debug disabled])
])

AC_OUTPUT
