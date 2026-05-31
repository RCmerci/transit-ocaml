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

  (** Convert a Transit value to an EDN value. Transit values without a native
      EDN representation are encoded as tagged EDN values. *)
  val to_edn : value -> Edn_ocaml.t

  (** Decode a Transit value from a Yojson value. *)
  val of_yojson : Yojson.Safe.t -> value

  (** Decode a Transit value from a JSON string. *)
  val of_string : string -> value

  (** Convert an EDN value to a Transit value. Recognized tagged EDN values are
      decoded back to their Transit-specific representations. *)
  val of_edn : Edn_ocaml.t -> value
end
