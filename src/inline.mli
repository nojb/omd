open Ast

type t = inline

val concat : t list -> t

val normalize : t -> t

module Pre : sig
  type inline

  type delim =
    | Ws
    | Punct
    | Other

  type t =
    | Bang_left_bracket
    | Left_bracket
    | Emph of delim * delim * emph_style * int
    | R of inline

  val is_opener : t -> bool
  val is_closer : t -> bool
  val classify_delim : char -> delim

  val parse_emph : t list -> inline list
end with type inline := t
