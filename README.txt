Under certain circumstances GHC generates incorrect code which goes into <<loop>>.

When you compile and run the following project without profiling and with eager black-holing, it will throw <<loop>> and exit.

If you compile either without eager black-holing or with profiling, it will run correctly.

This bug is complicated to trigger, inlining certain things will prevent it from triggering.
