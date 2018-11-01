{
let strbuf =
  Buffer.create 101

let add_char c =
  Buffer.add_char strbuf c

let add_string s =
  Buffer.add_string strbuf s

let add_lexeme lexbuf =
  add_string (Lexing.lexeme lexbuf)

let get_buf () =
  let s = Buffer.contents strbuf in
  Buffer.clear strbuf;
  s

let rewind lexbuf n =
  lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_curr_pos - n

let matching = function
  | '(' -> ')'
  | _ as c -> c

let copy_lexbuf
    { Lexing.refill_buff; lex_buffer; lex_buffer_len; lex_abs_pos;
      lex_start_pos; lex_curr_pos; lex_last_pos; lex_last_action;
      lex_eof_reached; lex_mem; lex_start_p; lex_curr_p }
  =
  { Lexing.refill_buff; lex_buffer; lex_buffer_len; lex_abs_pos;
    lex_start_pos; lex_curr_pos; lex_last_pos; lex_last_action;
    lex_eof_reached; lex_mem; lex_start_p; lex_curr_p }

let set_lexbuf lexbuf0 lexbuf =
  lexbuf.Lexing.lex_buffer <- lexbuf0.Lexing.lex_buffer;
  lexbuf.Lexing.lex_buffer_len <- lexbuf0.Lexing.lex_buffer_len;
  lexbuf.Lexing.lex_start_pos <- lexbuf0.Lexing.lex_start_pos;
  lexbuf.Lexing.lex_curr_pos <- lexbuf0.Lexing.lex_curr_pos;
  lexbuf.Lexing.lex_last_pos <- lexbuf0.Lexing.lex_last_pos;
  lexbuf.Lexing.lex_last_action <- lexbuf0.Lexing.lex_last_action;
  lexbuf.Lexing.lex_eof_reached <- lexbuf0.Lexing.lex_eof_reached;
  lexbuf.Lexing.lex_mem <- lexbuf0.Lexing.lex_mem;
  lexbuf.Lexing.lex_start_p <- lexbuf0.Lexing.lex_start_p;
  lexbuf.Lexing.lex_curr_p <- lexbuf0.Lexing.lex_curr_p

let protect c f lexbuf =
  let lexbuf0 = copy_lexbuf lexbuf in
  match f lexbuf with
  | x -> x
  | exception _ -> set_lexbuf lexbuf0 lexbuf; c
}

let punct =
  ['!''"''#''$''%''&''\'''('')''*''+'',''-'
   '.''/'':'';''<''=''>''?''@''[''\\'']''^'
   '_''`''{''|''}''~']
let ws =
  [' ''\t''\010'-'\013']
let sp3 =
  (' ' (' ' ' '?)?)?
let nl =
  '\n' | '\r'
let sp =
  ' '  | '\t'

rule destination = parse
  | '<' { destination1 lexbuf }
  | _   { rewind lexbuf 1; destination2 0 lexbuf }

and destination1 = parse
  | '\\' (punct as c)
      { add_char c; destination1 lexbuf }
  | '>'
      { get_buf () }
  | _ as c
      { add_char c; destination1 lexbuf }

and destination2 n = parse
  | '(' as c
      { add_char c; destination2 (succ n) lexbuf }
  | ')' as c
      { if n > 0 then
          (add_char c; destination2 (pred n) lexbuf)
        else
          (rewind lexbuf 1; get_buf ()) }
  | '\\' (punct as c)
      { add_char c; destination2 n lexbuf }
  | [^' ''\t''\x00'-'\x1F''\x7F''\x80'-'\x9F'] as c
      { add_char c; destination2 n lexbuf }
  | _
      { rewind lexbuf 1; get_buf () }
  | eof (* FIXME this is invalid for inline links *)
      { get_buf () }

(*
and ws k = parse
  | ws*
      { k lexbuf }
*)

and ws_destination = parse
  | ws* { destination lexbuf }

and title1 d = parse
  | '\\' (punct as c)
      { add_char c; title1 d lexbuf }
  | '"' | '\'' | ')' as c
      { if c = matching d then get_buf () else (add_char c; title1 d lexbuf) }
  | _ as c
      { add_char c; title1 d lexbuf }

and ws_right_paren = parse
  | ws* ')' { () }

and title_with_ws_for_reference = parse
  | ws+ ('"' | '\'' | '(' as c)
      { let t = title1 c lexbuf in
        ws_right_paren lexbuf; Some t }
  | ws* ')'
      { None }

and ws_eol = parse
  | sp* (nl | eof) { () }

and title_with_ws_for_definition = parse
  | (sp* nl sp*) ('"' | '\'' | '(' as c)
      { let title1 lexbuf = let s = title1 c lexbuf in ws_eol lexbuf; s in
        protect None (fun lexbuf -> Some (title1 lexbuf)) lexbuf }
  | sp* nl | sp* eof
      { None }
  | sp+ ('"' | '\'' | '(' as c)
      { let title1 lexbuf = let s = title1 c lexbuf in ws_eol lexbuf; s in
        Some (title1 lexbuf) }

and definition_label = parse
  | '\\' (punct as c) { add_char c; definition_label lexbuf }
  | ']' ':' { get_buf () }
  | _ as c { add_char c; definition_label lexbuf }

{
let destination_and_title_for_reference lexbuf =
  Buffer.clear strbuf;
  let dest = ws_destination lexbuf in
  let title = title_with_ws_for_reference lexbuf in
  dest, title

let definition lexbuf =
  Buffer.clear strbuf;
  let label = definition_label lexbuf in
  let destination = ws_destination lexbuf in
  let title = title_with_ws_for_definition lexbuf in
  {Ast.label; destination; title}
}