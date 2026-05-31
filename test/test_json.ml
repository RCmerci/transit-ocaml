module Json = Transit.Json
open Json

let checks_run = ref 0

let check name expected actual =
  incr checks_run;
  if not (String.equal expected actual) then
    failwith
      (Printf.sprintf "check failed: %s\nexpected: %s\nactual:   %s" name
         expected actual)

let write ?mode value = Json.to_string ?mode value
let read text = Json.from_string text

let check_value name expected actual =
  incr checks_run;
  if expected <> actual then
    failwith (Printf.sprintf "check failed: %s" name)

let check_float name expected = function
  | Float actual when Float.equal expected actual -> incr checks_run
  | _ -> failwith (Printf.sprintf "check failed: %s" name)

let test_ground_scalars () =
  check "null" "null" (write Null);
  check "true" "true" (write (Bool true));
  check "false" "false" (write (Bool false));
  check "safe int" "42" (write (Int 42));
  check "large int64" "\"~i9007199254740992\""
    (write (Int64 9_007_199_254_740_992L));
  check "float" "1.25" (write (Float 1.25));
  check "string" "\"hello\"" (write (String "hello"));
  check "string escapes tilde" "\"~~x\"" (write (String "~x"));
  check "string escapes caret" "\"~^x\"" (write (String "^x"));
  check "string escapes backtick" "\"~`x\"" (write (String "`x"))

let test_stringable_map_keys () =
  let value =
    Map
      [
        (Null, String "nil");
        (Bool true, String "yes");
        (Int 7, String "seven");
        (Float 1.5, String "one-five");
        (String "name", String "Ada");
      ]
  in
  check "map with stringable keys"
    "[\"^ \",\"~_\",\"nil\",\"~?t\",\"yes\",\"~i7\",\"seven\",\"~d1.5\",\"one-five\",\"name\",\"Ada\"]"
    (write value)

let test_keyword_symbol_and_key_caching () =
  check "keyword and symbol cache in normal mode"
    "[\"~:color\",\"^0\",\"~$thing\",\"^1\"]"
    (write
       (Array
          [
            Keyword "color";
            Keyword "color";
            Symbol "thing";
            Symbol "thing";
          ]));
  check "repeated string map keys cache in normal mode"
    "[\"^ \",\"name\",\"Ada\",\"^0\",\"Grace\"]"
    (write
       (Map
          [
            (String "name", String "Ada");
            (String "name", String "Grace");
          ]))

let test_extension_values () =
  check "bytes" "\"~baGk=\"" (write (Bytes "hi"));
  check "big decimal" "\"~f123.456\"" (write (Big_decimal "123.456"));
  check "big int" "\"~n12345678901234567890\""
    (write (Big_int "12345678901234567890"));
  check "time" "\"~m123456789\"" (write (Time 123_456_789L));
  check "uuid" "\"~u531a379e-31bb-4ce1-8690-158dceb64be6\""
    (write (Uuid "531a379e-31bb-4ce1-8690-158dceb64be6"));
  check "uri" "\"~rhttps://example.com\"" (write (Uri "https://example.com"));
  check "char" "\"~cx\"" (write (Char "x"));
  check "nan" "\"~zNaN\"" (write (Float Float.nan));
  check "inf" "\"~zINF\"" (write (Float infinity));
  check "neg inf" "\"~z-INF\"" (write (Float neg_infinity))

let test_composites () =
  check "array" "[1,\"two\",true]" (write (Array [ Int 1; String "two"; Bool true ]));
  check "set" "[\"~#set\",[\"a\",\"b\"]]" (write (Set [ String "a"; String "b" ]));
  check "list" "[\"~#list\",[\"a\"]]" (write (List [ String "a" ]));
  check "quote" "[\"~#'\",\"literal\"]" (write (Quote (String "literal")));
  check "tagged value" "[\"~#point\",[10,20]]"
    (write (Tagged ("point", Array [ Int 10; Int 20 ])))

let test_composite_key_map () =
  check "composite key map"
    "[\"~#cmap\",[[1,2],\"point\"]]"
    (write (Map [ (Array [ Int 1; Int 2 ], String "point") ]))

let test_verbose_mode () =
  check "verbose map object"
    "{\"name\":\"Ada\",\"~:role\":\"dev\"}"
    (write ~mode:Verbose
       (Map [ (String "name", String "Ada"); (Keyword "role", String "dev") ]));
  check "verbose disables cache"
    "[\"~:color\",\"~:color\"]"
    (write ~mode:Verbose (Array [ Keyword "color"; Keyword "color" ]))

let test_read_ground_scalars () =
  check_value "read null" Null (read "null");
  check_value "read true" (Bool true) (read "true");
  check_value "read false" (Bool false) (read "false");
  check_value "read int" (Int 42) (read "42");
  check_value "read large int64" (Int64 9_007_199_254_740_992L)
    (read "\"~i9007199254740992\"");
  check_float "read float" 1.25 (read "1.25");
  check_value "read string" (String "hello") (read "\"hello\"");
  check_value "read escaped tilde" (String "~x") (read "\"~~x\"");
  check_value "read escaped caret" (String "^x") (read "\"~^x\"");
  check_value "read escaped backtick" (String "`x") (read "\"~`x\"")

let test_read_string_tags () =
  check_value "read null tag" Null (read "\"~_\"");
  check_value "read true tag" (Bool true) (read "\"~?t\"");
  check_value "read false tag" (Bool false) (read "\"~?f\"");
  check_value "read int tag" (Int 7) (read "\"~i7\"");
  check_float "read float tag" 1.5 (read "\"~d1.5\"");
  check_value "read bytes" (Bytes "hi") (read "\"~baGk=\"");
  check_value "read keyword" (Keyword "color") (read "\"~:color\"");
  check_value "read symbol" (Symbol "thing") (read "\"~$thing\"");
  check_value "read big decimal" (Big_decimal "123.456") (read "\"~f123.456\"");
  check_value "read big int" (Big_int "12345678901234567890")
    (read "\"~n12345678901234567890\"");
  check_value "read time" (Time 123_456_789L) (read "\"~m123456789\"");
  check_value "read uuid" (Uuid "531a379e-31bb-4ce1-8690-158dceb64be6")
    (read "\"~u531a379e-31bb-4ce1-8690-158dceb64be6\"");
  check_value "read uri" (Uri "https://example.com")
    (read "\"~rhttps://example.com\"");
  check_value "read char" (Char "x") (read "\"~cx\"");
  check_float "read inf" infinity (read "\"~zINF\"");
  check_float "read neg inf" neg_infinity (read "\"~z-INF\"")

let test_read_maps () =
  check_value "read map-as-array"
    (Map
       [
         (Null, String "nil");
         (Bool true, String "yes");
         (Int 7, String "seven");
         (Float 1.5, String "one-five");
         (String "name", String "Ada");
       ])
    (read
       "[\"^ \",\"~_\",\"nil\",\"~?t\",\"yes\",\"~i7\",\"seven\",\"~d1.5\",\"one-five\",\"name\",\"Ada\"]");
  check_value "read verbose object"
    (Map [ (String "name", String "Ada"); (Keyword "role", String "dev") ])
    (read "{\"name\":\"Ada\",\"~:role\":\"dev\"}")

let test_read_cache () =
  check_value "read keyword cache"
    (Array [ Keyword "color"; Keyword "color"; Symbol "thing"; Symbol "thing" ])
    (read "[\"~:color\",\"^0\",\"~$thing\",\"^1\"]");
  check_value "read repeated string key cache"
    (Map [ (String "name", String "Ada"); (String "name", String "Grace") ])
    (read "[\"^ \",\"name\",\"Ada\",\"^0\",\"Grace\"]")

let test_read_composites () =
  check_value "read array" (Array [ Int 1; String "two"; Bool true ])
    (read "[1,\"two\",true]");
  check_value "read set" (Set [ String "a"; String "b" ])
    (read "[\"~#set\",[\"a\",\"b\"]]");
  check_value "read list" (List [ String "a" ]) (read "[\"~#list\",[\"a\"]]");
  check_value "read quote" (Quote (String "literal"))
    (read "[\"~#'\",\"literal\"]");
  check_value "read tagged value" (Tagged ("point", Array [ Int 10; Int 20 ]))
    (read "[\"~#point\",[10,20]]");
  check_value "read composite key map"
    (Map [ (Array [ Int 1; Int 2 ], String "point") ])
    (read "[\"~#cmap\",[[1,2],\"point\"]]");
  check_value "read verbose tagged value" (Tagged ("point", Array [ Int 10; Int 20 ]))
    (read "{\"~#point\":[10,20]}")

let () =
  test_ground_scalars ();
  test_stringable_map_keys ();
  test_keyword_symbol_and_key_caching ();
  test_extension_values ();
  test_composites ();
  test_composite_key_map ();
  test_verbose_mode ();
  test_read_ground_scalars ();
  test_read_string_tags ();
  test_read_maps ();
  test_read_cache ();
  test_read_composites ();
  Printf.printf "ok - %d checks\n" !checks_run
