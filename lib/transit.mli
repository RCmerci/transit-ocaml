module Json : sig
  (** Transit JSON write mode. [Normal] enables Transit caching and writes maps
      with the ["^ "] map-as-array marker. [Verbose] disables caching and writes
      stringable maps as JSON objects. *)
  type mode =
    | Normal
    | Verbose

  (** Transit semantic values supported by the JSON reader and writer. *)
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

  exception Decode_error of string

  (** Encode a Transit value as a Yojson value. *)
  val to_yojson : ?mode:mode -> value -> Yojson.Safe.t

  (** Encode a Transit value as a JSON string. *)
  val to_string : ?mode:mode -> value -> string

  (** Decode a Transit value from a Yojson value. *)
  val from_yojson : Yojson.Safe.t -> value

  (** Decode a Transit value from a JSON string. *)
  val from_string : string -> value
end
