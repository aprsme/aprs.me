import gleam/string
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/int
import gleam/float
import gleam/bit_array

/// Sanitizes a binary to ensure it can be safely JSON encoded
/// Handles latin1 conversion and removes control characters
pub fn sanitize_string(input: BitArray) -> String {
  // First try direct UTF-8 conversion
  case bit_array.to_string(input) {
    Ok(s) -> clean_control_characters(s)
    Error(_) -> {
      // Try latin1 to UTF-8 conversion
      input
      |> latin1_to_utf8_string
      |> clean_control_characters
    }
  }
}

/// Convert BitArray to list of bytes
fn bit_array_to_list(input: BitArray) -> List(Int) {
  do_bit_array_to_list(input, [])
  |> list.reverse
}

fn do_bit_array_to_list(input: BitArray, acc: List(Int)) -> List(Int) {
  case bit_array.byte_size(input) {
    0 -> acc
    _ -> {
      case bit_array.slice(input, at: 0, take: 1) {
        Ok(<<byte>>) -> {
          case bit_array.slice(input, at: 1, take: bit_array.byte_size(input) - 1) {
            Ok(rest) -> do_bit_array_to_list(rest, [byte, ..acc])
            Error(_) -> [byte, ..acc]
          }
        }
        _ -> acc
      }
    }
  }
}


/// Convert latin1 encoded bytes to UTF-8 string
fn latin1_to_utf8_string(input: BitArray) -> String {
  input
  |> bit_array_to_list
  |> list.filter_map(fn(byte) {
    case byte {
      // ASCII range (0-127) maps directly
      b if b <= 127 -> {
        case bit_array.to_string(<<b>>) {
          Ok(s) -> Ok(s)
          Error(_) -> Error(Nil)
        }
      }
      // Latin1 range (128-255) needs UTF-8 encoding
      b -> {
        // For Latin1, values 128-255 map to Unicode U+0080 to U+00FF
        // In UTF-8, these become 2-byte sequences: 110xxxxx 10xxxxxx
        let byte1 = 192 + b / 64  // 192 = 0xC0, equivalent to b >>> 6
        let byte2 = 128 + b % 64  // 128 = 0x80, equivalent to b & 0x3F
        case bit_array.to_string(<<byte1, byte2>>) {
          Ok(s) -> Ok(s)
          Error(_) -> Error(Nil)
        }
      }
    }
  })
  |> string.join("")
}

/// Remove control characters from a string
fn clean_control_characters(s: String) -> String {
  s
  |> string.to_graphemes
  |> list.filter(fn(grapheme) {
    case string.to_utf_codepoints(grapheme) {
      [codepoint] -> {
        let cp = string.utf_codepoint_to_int(codepoint)
        // Allow tab (0x09), newline (0x0A), carriage return (0x0D)
        // Remove other control characters
        case cp {
          9 -> True
          10 -> True
          13 -> True
          c if c >= 0 && c <= 31 -> False
          127 -> False
          c if c >= 128 && c <= 159 -> False
          _ -> True
        }
      }
      _ -> True  // Multi-codepoint grapheme, keep it
    }
  })
  |> string.join("")
  |> string.trim
}

/// Type-safe float conversion with validation
pub fn to_float_safe(value: String) -> Option(Float) {
  let sanitized = value
    |> string.trim
    |> string.slice(0, 30)  // Reasonable max length for a number
    
  case float.parse(sanitized) {
    Ok(f) -> {
      // Check for reasonable bounds
      case f {
        x if x >. -9.0e15 && x <. 9.0e15 -> Some(f)
        _ -> None
      }
    }
    Error(_) -> None
  }
}

/// Convert binary to hex string
pub fn to_hex(input: BitArray) -> String {
  do_to_hex(input, [])
  |> list.reverse
  |> string.join("")
  |> string.uppercase
}

fn do_to_hex(input: BitArray, acc: List(String)) -> List(String) {
  case bit_array.slice(input, at: 0, take: 1) {
    Ok(<<byte>>) -> {
      let rest = case bit_array.slice(input, at: 1, take: bit_array.byte_size(input) - 1) {
        Ok(r) -> r
        Error(_) -> <<>>
      }
      let hex = int.to_base16(byte)
      let padded = case string.length(hex) {
        1 -> "0" <> hex
        _ -> hex
      }
      do_to_hex(rest, [padded, ..acc])
    }
    _ -> acc
  }
}

/// Check if a value looks like it has weather data
pub fn has_weather_data(temperature: Option(Float), humidity: Option(Float), 
                       wind_speed: Option(Float), pressure: Option(Float)) -> Bool {
  case temperature, humidity, wind_speed, pressure {
    Some(_), _, _, _ -> True
    _, Some(_), _, _ -> True
    _, _, Some(_), _ -> True
    _, _, _, Some(_) -> True
    _, _, _, _ -> False
  }
}

/// Encoding info for debugging
pub type EncodingInfo {
  EncodingInfo(
    valid_utf8: Bool,
    byte_count: Int,
    char_count: Option(Int),
    invalid_at: Option(Int)
  )
}

/// Get encoding information about a binary
pub fn encoding_info(input: BitArray) -> EncodingInfo {
  let byte_count = bit_array.byte_size(input)
  
  case bit_array.to_string(input) {
    Ok(s) -> EncodingInfo(
      valid_utf8: True,
      byte_count: byte_count,
      char_count: Some(string.length(s)),
      invalid_at: None
    )
    Error(_) -> {
      let invalid_pos = find_invalid_byte_position(input, 0)
      EncodingInfo(
        valid_utf8: False,
        byte_count: byte_count,
        char_count: None,
        invalid_at: invalid_pos
      )
    }
  }
}

fn find_invalid_byte_position(input: BitArray, pos: Int) -> Option(Int) {
  case bit_array.byte_size(input) {
    0 -> None
    _ -> {
      case bit_array.slice(input, at: 0, take: 1) {
        Ok(byte_slice) -> {
          case bit_array.to_string(byte_slice) {
            Ok(_) -> {
              case bit_array.slice(input, at: 1, take: bit_array.byte_size(input) - 1) {
                Ok(rest) -> find_invalid_byte_position(rest, pos + 1)
                Error(_) -> Some(pos)
              }
            }
            Error(_) -> Some(pos)
          }
        }
        Error(_) -> Some(pos)
      }
    }
  }
}