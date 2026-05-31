module Json = struct
  type mode =
    | Normal
    | Verbose

  type value =
    | Null
    | Bool of bool
    | String of string
    | Int of int
    | Int64 of int64
    | Float of float
    | Bytes of string
    | Keyword of string
    | Symbol of string
    | Big_decimal of string
    | Big_int of string
    | Time of int64
    | Uuid of string
    | Uri of string
    | Char of string
    | Array of value list
    | Map of (value * value) list
    | Set of value list
    | List of value list
    | Quote of value
    | Tagged of string * value

  type writer = {
    mode : mode;
    cache : (string, string) Hashtbl.t;
    mutable next_cache_index : int;
  }

  let cache_code_digits = 44
  let cache_size = cache_code_digits * cache_code_digits
  let base_char_code = Char.code '0'

  let index_to_cache_code index =
    let hi = index / cache_code_digits in
    let lo = index mod cache_code_digits in
    if hi = 0 then Printf.sprintf "^%c" (Char.chr (lo + base_char_code))
    else
      Printf.sprintf "^%c%c"
        (Char.chr (hi + base_char_code))
        (Char.chr (lo + base_char_code))

  let cacheable_string writer text =
    match writer.mode with
    | Verbose -> text
    | Normal -> (
        if String.length text <= 3 then text
        else
          match Hashtbl.find_opt writer.cache text with
          | Some code -> code
          | None ->
              if writer.next_cache_index >= cache_size then (
                Hashtbl.clear writer.cache;
                writer.next_cache_index <- 0);
              let code = index_to_cache_code writer.next_cache_index in
              writer.next_cache_index <- writer.next_cache_index + 1;
              Hashtbl.add writer.cache text code;
              text)

  let escape_string text =
    if String.length text = 0 then text
    else
      match text.[0] with
      | '~' | '^' | '`' -> "~" ^ text
      | _ -> text

  let max_safe_json_int = 9_007_199_254_740_992L

  let is_safe_json_int value =
    Int64.compare value (Int64.neg max_safe_json_int) > 0
    && Int64.compare value max_safe_json_int < 0

  let yojson_int value =
    if is_safe_json_int value then `Int (Int64.to_int value)
    else `String ("~i" ^ Int64.to_string value)

  let float_rep value = Yojson.Safe.to_string (`Float value)

  let special_float_string value =
    match classify_float value with
    | FP_nan -> Some "~zNaN"
    | FP_infinite when value > 0. -> Some "~zINF"
    | FP_infinite -> Some "~z-INF"
    | FP_normal | FP_subnormal | FP_zero -> None

  let base64_encode text =
    let alphabet =
      "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
    in
    let len = String.length text in
    let output = Buffer.create (((len + 2) / 3) * 4) in
    let add index = Buffer.add_char output alphabet.[index] in
    let rec loop offset =
      if offset < len then (
        let b0 = Char.code text.[offset] in
        let has_b1 = offset + 1 < len in
        let has_b2 = offset + 2 < len in
        let b1 = if has_b1 then Char.code text.[offset + 1] else 0 in
        let b2 = if has_b2 then Char.code text.[offset + 2] else 0 in
        add (b0 lsr 2);
        add (((b0 land 0x03) lsl 4) lor (b1 lsr 4));
        if has_b1 then add (((b1 land 0x0f) lsl 2) lor (b2 lsr 6))
        else Buffer.add_char output '=';
        if has_b2 then add (b2 land 0x3f) else Buffer.add_char output '=';
        loop (offset + 3))
    in
    loop 0;
    Buffer.contents output

  let scalar_string tag rep = "~" ^ tag ^ rep

  let tagged_string writer tag =
    cacheable_string writer ("~#" ^ tag)

  let keyword_string writer text =
    cacheable_string writer (scalar_string ":" text)

  let symbol_string writer text =
    cacheable_string writer (scalar_string "$" text)

  let rec is_stringable_key = function
    | Null | Bool _ | String _ | Int _ | Int64 _ | Float _ | Bytes _ | Keyword _
    | Symbol _ | Big_decimal _ | Big_int _ | Time _ | Uuid _ | Uri _ | Char _ ->
        true
    | Array _ | Map _ | Set _ | List _ | Quote _ | Tagged _ -> false

  and key_to_string writer ?(cache_string_key = false) = function
    | Null -> "~_"
    | Bool true -> "~?t"
    | Bool false -> "~?f"
    | String text ->
        let text = escape_string text in
        if cache_string_key then cacheable_string writer text else text
    | Int value -> scalar_string "i" (Int.to_string value)
    | Int64 value -> scalar_string "i" (Int64.to_string value)
    | Float value -> (
        match special_float_string value with
        | Some text -> text
        | None -> scalar_string "d" (float_rep value))
    | Bytes text -> scalar_string "b" (base64_encode text)
    | Keyword text -> keyword_string writer text
    | Symbol text -> symbol_string writer text
    | Big_decimal text -> scalar_string "f" text
    | Big_int text -> scalar_string "n" text
    | Time milliseconds -> scalar_string "m" (Int64.to_string milliseconds)
    | Uuid text -> scalar_string "u" text
    | Uri text -> scalar_string "r" text
    | Char text -> scalar_string "c" text
    | Array _ | Map _ | Set _ | List _ | Quote _ | Tagged _ ->
        invalid_arg "Transit.Json.key_to_string: composite key"

  and write writer = function
    | Null -> `Null
    | Bool value -> `Bool value
    | String text -> `String (escape_string text)
    | Int value -> yojson_int (Int64.of_int value)
    | Int64 value -> yojson_int value
    | Float value -> (
        match special_float_string value with
        | Some text -> `String text
        | None -> `Float value)
    | Bytes text -> `String (scalar_string "b" (base64_encode text))
    | Keyword text -> `String (keyword_string writer text)
    | Symbol text -> `String (symbol_string writer text)
    | Big_decimal text -> `String (scalar_string "f" text)
    | Big_int text -> `String (scalar_string "n" text)
    | Time milliseconds -> `String (scalar_string "m" (Int64.to_string milliseconds))
    | Uuid text -> `String (scalar_string "u" text)
    | Uri text -> `String (scalar_string "r" text)
    | Char text -> `String (scalar_string "c" text)
    | Array values -> `List (List.map (write writer) values)
    | Map entries -> write_map writer entries
    | Set values ->
        write_composite writer "set" (fun () ->
            `List (List.map (write writer) values))
    | List values ->
        write_composite writer "list" (fun () ->
            `List (List.map (write writer) values))
    | Quote value -> write_composite writer "'" (fun () -> write writer value)
    | Tagged (tag, value) -> write_composite writer tag (fun () -> write writer value)

  and write_map writer entries =
    if List.for_all (fun (key, _) -> is_stringable_key key) entries then
      match writer.mode with
      | Normal ->
          let elements =
            entries
            |> List.concat_map (fun (key, value) ->
                   let key_json =
                     `String (key_to_string writer ~cache_string_key:true key)
                   in
                   let value_json = write writer value in
                   [ key_json; value_json ])
          in
          `List (`String "^ " :: elements)
      | Verbose ->
          `Assoc
            (List.map
               (fun (key, value) ->
                 let key_json = key_to_string writer key in
                 let value_json = write writer value in
                 (key_json, value_json))
               entries)
    else
      write_composite writer "cmap" (fun () ->
          let flattened =
            entries
            |> List.concat_map (fun (key, value) ->
                   let key_json = write writer key in
                   let value_json = write writer value in
                   [ key_json; value_json ])
          in
          `List flattened)

  and write_composite writer tag rep =
    let tag = tagged_string writer tag in
    let rep = rep () in
    match writer.mode with
    | Normal -> `List [ `String tag; rep ]
    | Verbose -> `Assoc [ (tag, rep) ]

  let to_yojson ?(mode = Normal) value =
    let writer = { mode; cache = Hashtbl.create 32; next_cache_index = 0 } in
    write writer value

  let to_string ?mode value = to_yojson ?mode value |> Yojson.Safe.to_string

  type reader = { mutable cache : string array }

  exception Decode_error of string

  let decode_error message = raise (Decode_error message)

  let cache_code_to_index text =
    match String.length text with
    | 2 -> Char.code text.[1] - base_char_code
    | 3 ->
        ((Char.code text.[1] - base_char_code) * cache_code_digits)
        + (Char.code text.[2] - base_char_code)
    | _ -> decode_error ("invalid cache code: " ^ text)

  let reader_cacheable text = String.length text > 3

  let remember reader text =
    if reader_cacheable text then (
      let len = Array.length reader.cache in
      if len >= cache_size then reader.cache <- [||];
      reader.cache <- Array.append reader.cache [| text |])

  let lookup_cache reader text =
    let index = cache_code_to_index text in
    if index < 0 || index >= Array.length reader.cache then
      decode_error ("unknown cache code: " ^ text)
    else reader.cache.(index)

  let is_cache_code text =
    String.length text >= 2 && String.length text <= 3 && text.[0] = '^'
    && not (String.equal text "^ ")

  let int_value text =
    match Int64.of_string_opt text with
    | Some value when is_safe_json_int value -> (
        match int_of_string_opt text with
        | Some value -> Int value
        | None -> Int64 value)
    | Some value -> Int64 value
    | None -> decode_error ("invalid integer: " ^ text)

  let int64_value text =
    match Int64.of_string_opt text with
    | Some value -> value
    | None -> decode_error ("invalid int64: " ^ text)

  let base64_value character =
    match character with
    | 'A' .. 'Z' -> Char.code character - Char.code 'A'
    | 'a' .. 'z' -> Char.code character - Char.code 'a' + 26
    | '0' .. '9' -> Char.code character - Char.code '0' + 52
    | '+' -> 62
    | '/' -> 63
    | _ -> decode_error "invalid base64 character"

  let base64_decode text =
    let len = String.length text in
    if len mod 4 <> 0 then decode_error "invalid base64 length";
    let output = Buffer.create ((len / 4) * 3) in
    let rec loop offset =
      if offset < len then (
        let c0 = text.[offset] in
        let c1 = text.[offset + 1] in
        let c2 = text.[offset + 2] in
        let c3 = text.[offset + 3] in
        let v0 = base64_value c0 in
        let v1 = base64_value c1 in
        let v2 = if c2 = '=' then 0 else base64_value c2 in
        let v3 = if c3 = '=' then 0 else base64_value c3 in
        Buffer.add_char output (Char.chr ((v0 lsl 2) lor (v1 lsr 4)));
        if c2 <> '=' then
          Buffer.add_char output (Char.chr (((v1 land 0x0f) lsl 4) lor (v2 lsr 2)));
        if c3 <> '=' then
          Buffer.add_char output (Char.chr (((v2 land 0x03) lsl 6) lor v3));
        loop (offset + 4))
    in
    loop 0;
    Buffer.contents output

  let drop_prefix text =
    String.sub text 2 (String.length text - 2)

  let rec read_string reader ?(cache_string_key = false) ?(remember_value = true) text =
    if is_cache_code text then
      read_string reader ~cache_string_key ~remember_value:false (lookup_cache reader text)
    else if String.length text = 0 then String text
    else if text.[0] <> '~' then (
      if cache_string_key && remember_value then remember reader text;
      String text)
    else if String.length text = 1 then String text
    else
      match text.[1] with
      | '~' | '^' | '`' -> String (String.sub text 1 (String.length text - 1))
      | '_' when String.length text = 2 -> Null
      | '?' -> (
          match drop_prefix text with
          | "t" -> Bool true
          | "f" -> Bool false
          | value -> decode_error ("invalid boolean: " ^ value))
      | 'i' -> int_value (drop_prefix text)
      | 'd' -> Float (float_of_string (drop_prefix text))
      | 'b' -> Bytes (base64_decode (drop_prefix text))
      | ':' ->
          if remember_value then remember reader text;
          Keyword (drop_prefix text)
      | '$' ->
          if remember_value then remember reader text;
          Symbol (drop_prefix text)
      | 'f' -> Big_decimal (drop_prefix text)
      | 'n' -> Big_int (drop_prefix text)
      | 'm' -> Time (int64_value (drop_prefix text))
      | 'u' -> Uuid (drop_prefix text)
      | 'r' -> Uri (drop_prefix text)
      | 'c' -> Char (drop_prefix text)
      | 'z' -> (
          match drop_prefix text with
          | "NaN" -> Float Float.nan
          | "INF" -> Float infinity
          | "-INF" -> Float neg_infinity
          | value -> decode_error ("invalid special number: " ^ value))
      | _ -> String text

  and read_list reader values =
    match values with
    | [] -> []
    | value :: rest ->
        let value = read reader value in
        value :: read_list reader rest

  and read_key reader json =
    match json with
    | `String text -> read_string reader ~cache_string_key:true text
    | _ -> read reader json

  and read_tag reader text =
    let remember_value = not (is_cache_code text) in
    let text = if is_cache_code text then lookup_cache reader text else text in
    if String.length text >= 3 && String.sub text 0 2 = "~#" then (
      if remember_value then remember reader text;
      String.sub text 2 (String.length text - 2))
    else decode_error ("invalid tag: " ^ text)

  and read_composite reader tag rep =
    match (tag, rep) with
    | "set", `List values -> Set (read_list reader values)
    | "list", `List values -> List (read_list reader values)
    | "'", value -> Quote (read reader value)
    | "cmap", `List values -> Map (read_flat_entries reader values)
    | "m", value -> Time (read_time_rep reader value)
    | tag, value -> Tagged (tag, read reader value)

  and read_flat_entries reader = function
    | [] -> []
    | key :: value :: rest ->
        let key = read reader key in
        let value = read reader value in
        let rest = read_flat_entries reader rest in
        (key, value) :: rest
    | _ -> decode_error "map requires an even number of elements"

  and read_time_rep reader = function
    | `Int value -> Int64.of_int value
    | `Intlit value -> int64_value value
    | json -> (
        match read reader json with
        | Int value -> Int64.of_int value
        | Int64 value -> value
        | _ -> decode_error "time rep must be an integer")

  and read_map_array reader = function
    | `String "^ " :: entries -> Map (read_stringable_entries reader entries)
    | values -> Array (read_list reader values)

  and read_stringable_entries reader = function
    | [] -> []
    | key :: value :: rest ->
        let key = read_key reader key in
        let value = read reader value in
        let rest = read_stringable_entries reader rest in
        (key, value) :: rest
    | _ -> decode_error "map-as-array requires an even number of elements"

  and read_assoc reader entries =
    match entries with
    | [ (tag, rep) ] when String.length tag >= 2 && tag.[0] = '~' && tag.[1] = '#' ->
        read_composite reader (read_tag reader tag) rep
    | _ -> Map (read_assoc_entries reader entries)

  and read_assoc_entries reader = function
    | [] -> []
    | (key, value) :: rest ->
        let key = read_string reader key in
        let value = read reader value in
        (key, value) :: read_assoc_entries reader rest

  and read reader = function
    | `Null -> Null
    | `Bool value -> Bool value
    | `Int value -> Int value
    | `Intlit value -> int_value value
    | `Float value -> Float value
    | `Floatlit value -> Float (float_of_string value)
    | `String text -> read_string reader text
    | `List [ `String tag; rep ]
      when String.length tag >= 2 && (tag.[0] = '^' || (tag.[0] = '~' && tag.[1] = '#')) ->
        read_composite reader (read_tag reader tag) rep
    | `List values -> read_map_array reader values
    | `Assoc entries -> read_assoc reader entries
    | `Tuple values -> Array (read_list reader values)
    | `Variant (tag, None) -> Tagged (tag, Null)
    | `Variant (tag, Some value) -> Tagged (tag, read reader value)

  let from_yojson json = read { cache = [||] } json
  let from_string text = Yojson.Safe.from_string text |> from_yojson
end
