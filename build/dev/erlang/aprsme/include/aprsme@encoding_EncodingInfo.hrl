-record(encoding_info, {
    valid_utf8 :: boolean(),
    byte_count :: integer(),
    char_count :: gleam@option:option(integer()),
    invalid_at :: gleam@option:option(integer())
}).
