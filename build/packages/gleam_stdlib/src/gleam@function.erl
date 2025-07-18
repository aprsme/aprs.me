-module(gleam@function).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).
-define(FILEPATH, "src/gleam/function.gleam").
-export([identity/1, tap/2]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

-file("src/gleam/function.gleam", 3).
?DOC(" Takes a single argument and always returns its input value.\n").
-spec identity(CNY) -> CNY.
identity(X) ->
    X.

-file("src/gleam/function.gleam", 12).
?DOC(
    " Takes an argument and a single function, calls that function with that\n"
    " argument and returns that argument instead of the function return value.\n"
    "\n"
    " Useful for running synchronous side effects in a pipeline.\n"
).
-spec tap(CNZ, fun((CNZ) -> any())) -> CNZ.
tap(Arg, Effect) ->
    Effect(Arg),
    Arg.
