-module(aprsme@encoding).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).
-define(FILEPATH, "src/aprsme/encoding.gleam").
-export([sanitize_string/1, to_float_safe/1, to_hex/1, has_weather_data/4, encoding_info/1]).
-export_type([encoding_info/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

-type encoding_info() :: {encoding_info,
        boolean(),
        integer(),
        gleam@option:option(integer()),
        gleam@option:option(integer())}.

-file("src/aprsme/encoding.gleam", 29).
-spec do_bit_array_to_list(bitstring(), list(integer())) -> list(integer()).
do_bit_array_to_list(Input, Acc) ->
    case erlang:byte_size(Input) of
        0 ->
            Acc;

        _ ->
            case gleam_stdlib:bit_array_slice(Input, 0, 1) of
                {ok, <<Byte>>} ->
                    case gleam_stdlib:bit_array_slice(
                        Input,
                        1,
                        erlang:byte_size(Input) - 1
                    ) of
                        {ok, Rest} ->
                            do_bit_array_to_list(Rest, [Byte | Acc]);

                        {error, _} ->
                            [Byte | Acc]
                    end;

                _ ->
                    Acc
            end
    end.

-file("src/aprsme/encoding.gleam", 24).
?DOC(" Convert BitArray to list of bytes\n").
-spec bit_array_to_list(bitstring()) -> list(integer()).
bit_array_to_list(Input) ->
    _pipe = do_bit_array_to_list(Input, []),
    lists:reverse(_pipe).

-file("src/aprsme/encoding.gleam", 48).
?DOC(" Convert latin1 encoded bytes to UTF-8 string\n").
-spec latin1_to_utf8_string(bitstring()) -> binary().
latin1_to_utf8_string(Input) ->
    _pipe = Input,
    _pipe@1 = bit_array_to_list(_pipe),
    _pipe@2 = gleam@list:filter_map(_pipe@1, fun(Byte) -> case Byte of
                B when B =< 127 ->
                    case gleam@bit_array:to_string(<<B>>) of
                        {ok, S} ->
                            {ok, S};

                        {error, _} ->
                            {error, nil}
                    end;

                B@1 ->
                    Byte1 = 192 + (B@1 div 64),
                    Byte2 = 128 + (B@1 rem 64),
                    case gleam@bit_array:to_string(<<Byte1, Byte2>>) of
                        {ok, S@1} ->
                            {ok, S@1};

                        {error, _} ->
                            {error, nil}
                    end
            end end),
    gleam@string:join(_pipe@2, <<""/utf8>>).

-file("src/aprsme/encoding.gleam", 77).
?DOC(" Remove control characters from a string\n").
-spec clean_control_characters(binary()) -> binary().
clean_control_characters(S) ->
    _pipe = S,
    _pipe@1 = gleam@string:to_graphemes(_pipe),
    _pipe@2 = gleam@list:filter(
        _pipe@1,
        fun(Grapheme) -> case gleam@string:to_utf_codepoints(Grapheme) of
                [Codepoint] ->
                    Cp = gleam_stdlib:identity(Codepoint),
                    case Cp of
                        9 ->
                            true;

                        10 ->
                            true;

                        13 ->
                            true;

                        C when (C >= 0) andalso (C =< 31) ->
                            false;

                        127 ->
                            false;

                        C@1 when (C@1 >= 128) andalso (C@1 =< 159) ->
                            false;

                        _ ->
                            true
                    end;

                _ ->
                    true
            end end
    ),
    _pipe@3 = gleam@string:join(_pipe@2, <<""/utf8>>),
    gleam@string:trim(_pipe@3).

-file("src/aprsme/encoding.gleam", 10).
?DOC(
    " Sanitizes a binary to ensure it can be safely JSON encoded\n"
    " Handles latin1 conversion and removes control characters\n"
).
-spec sanitize_string(bitstring()) -> binary().
sanitize_string(Input) ->
    case gleam@bit_array:to_string(Input) of
        {ok, S} ->
            clean_control_characters(S);

        {error, _} ->
            _pipe = Input,
            _pipe@1 = latin1_to_utf8_string(_pipe),
            clean_control_characters(_pipe@1)
    end.

-file("src/aprsme/encoding.gleam", 104).
?DOC(" Type-safe float conversion with validation\n").
-spec to_float_safe(binary()) -> gleam@option:option(float()).
to_float_safe(Value) ->
    Sanitized = begin
        _pipe = Value,
        _pipe@1 = gleam@string:trim(_pipe),
        gleam@string:slice(_pipe@1, 0, 30)
    end,
    case gleam_stdlib:parse_float(Sanitized) of
        {ok, F} ->
            case F of
                X when (X > -9.0e15) andalso (X < 9.0e15) ->
                    {some, F};

                _ ->
                    none
            end;

        {error, _} ->
            none
    end.

-file("src/aprsme/encoding.gleam", 129).
-spec do_to_hex(bitstring(), list(binary())) -> list(binary()).
do_to_hex(Input, Acc) ->
    case gleam_stdlib:bit_array_slice(Input, 0, 1) of
        {ok, <<Byte>>} ->
            Rest = case gleam_stdlib:bit_array_slice(
                Input,
                1,
                erlang:byte_size(Input) - 1
            ) of
                {ok, R} ->
                    R;

                {error, _} ->
                    <<>>
            end,
            Hex = gleam@int:to_base16(Byte),
            Padded = case string:length(Hex) of
                1 ->
                    <<"0"/utf8, Hex/binary>>;

                _ ->
                    Hex
            end,
            do_to_hex(Rest, [Padded | Acc]);

        _ ->
            Acc
    end.

-file("src/aprsme/encoding.gleam", 122).
?DOC(" Convert binary to hex string\n").
-spec to_hex(bitstring()) -> binary().
to_hex(Input) ->
    _pipe = do_to_hex(Input, []),
    _pipe@1 = lists:reverse(_pipe),
    _pipe@2 = gleam@string:join(_pipe@1, <<""/utf8>>),
    string:uppercase(_pipe@2).

-file("src/aprsme/encoding.gleam", 148).
?DOC(" Check if a value looks like it has weather data\n").
-spec has_weather_data(
    gleam@option:option(float()),
    gleam@option:option(float()),
    gleam@option:option(float()),
    gleam@option:option(float())
) -> boolean().
has_weather_data(Temperature, Humidity, Wind_speed, Pressure) ->
    case {Temperature, Humidity, Wind_speed, Pressure} of
        {{some, _}, _, _, _} ->
            true;

        {_, {some, _}, _, _} ->
            true;

        {_, _, {some, _}, _} ->
            true;

        {_, _, _, {some, _}} ->
            true;

        {_, _, _, _} ->
            false
    end.

-file("src/aprsme/encoding.gleam", 192).
-spec find_invalid_byte_position(bitstring(), integer()) -> gleam@option:option(integer()).
find_invalid_byte_position(Input, Pos) ->
    case erlang:byte_size(Input) of
        0 ->
            none;

        _ ->
            case gleam_stdlib:bit_array_slice(Input, 0, 1) of
                {ok, Byte_slice} ->
                    case gleam@bit_array:to_string(Byte_slice) of
                        {ok, _} ->
                            case gleam_stdlib:bit_array_slice(
                                Input,
                                1,
                                erlang:byte_size(Input) - 1
                            ) of
                                {ok, Rest} ->
                                    find_invalid_byte_position(Rest, Pos + 1);

                                {error, _} ->
                                    {some, Pos}
                            end;

                        {error, _} ->
                            {some, Pos}
                    end;

                {error, _} ->
                    {some, Pos}
            end
    end.

-file("src/aprsme/encoding.gleam", 170).
?DOC(" Get encoding information about a binary\n").
-spec encoding_info(bitstring()) -> encoding_info().
encoding_info(Input) ->
    Byte_count = erlang:byte_size(Input),
    case gleam@bit_array:to_string(Input) of
        {ok, S} ->
            {encoding_info, true, Byte_count, {some, string:length(S)}, none};

        {error, _} ->
            Invalid_pos = find_invalid_byte_position(Input, 0),
            {encoding_info, false, Byte_count, none, Invalid_pos}
    end.
