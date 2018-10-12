{
open Representation

let delim lexbuf d =
  Delim (Lexing.lexeme_end lexbuf - Lexing.lexeme_start lexbuf, d)
}

let char = [^' ''\t''\n''\r''#''*''-''+''`''\'''"''\\''_'
             '['']''{''}''('')'':'';''>''~''<''@''&''|'
             '^''.''/''$''%''!''?''=']
let digit = ['0'-'9']
let word = char+ (char # digit) char* | (char # digit) char*

rule token = parse
| ' '+          { delim lexbuf Space }
| '\t'+         { Delim (String.length (Lexing.lexeme lexbuf) * 4, Space) }
| "\r\n"+       { Delim (String.length (Lexing.lexeme lexbuf) / 2, Newline) }
| ('\n'|'\r')+  { delim lexbuf Newline }
| '#'+          { delim lexbuf Hash }
| '*'+          { delim lexbuf Star }
| '-'+          { delim lexbuf Minus }
| '+'+          { delim lexbuf Plus }
| '`'+          { delim lexbuf Backquote }
| '\''+         { delim lexbuf Quote }
| '"'+          { delim lexbuf Doublequote }
| '\\'+         { delim lexbuf Backslash }
| '_'+          { delim lexbuf Underscore }
| '['+          { delim lexbuf Obracket }
| ']'+          { delim lexbuf Cbracket }
| '{'+          { delim lexbuf Obrace }
| '}'+          { delim lexbuf Cbrace }
| '('+          { delim lexbuf Oparenthesis }
| ')'+          { delim lexbuf Cparenthesis }
| ':'+          { delim lexbuf Colon }
| ';'+          { delim lexbuf Semicolon }
| '>'+          { delim lexbuf Greaterthan }
| '~'+          { delim lexbuf Tilde }
| '<'+          { delim lexbuf Lessthan }
| '@'+          { delim lexbuf At }
| '&'+          { delim lexbuf Ampersand }
| '|'+          { delim lexbuf Bar }
| '^'+          { delim lexbuf Caret }
| ','+          { delim lexbuf Comma }
| '.'+          { delim lexbuf Dot }
| '/'+          { delim lexbuf Slash }
| '$'+          { delim lexbuf Dollar }
| '%'+          { delim lexbuf Percent }
| '='+          { delim lexbuf Equal }
| '!'+          { delim lexbuf Exclamation }
| '?'+          { delim lexbuf Question }
| word          { Word (Lexing.lexeme lexbuf) }
| digit+        { Number (Lexing.lexeme lexbuf) }
| eof           { raise End_of_file }

{
type token = tok
type t = tok list

let char_of_delim = function
  | Ampersand -> '&'
  | At -> '@'
  | Backquote -> '`'
  | Backslash -> '\\'
  | Bar -> '|'
  | Caret -> '^'
  | Cbrace -> '}'
  | Colon -> ':'
  | Comma -> ','
  | Cparenthesis -> ')'
  | Cbracket -> ']'
  | Dollar -> '$'
  | Dot -> '.'
  | Doublequote -> '\"'
  | Exclamation -> '!'
  | Equal -> '='
  | Greaterthan -> '>'
  | Hash -> '#'
  | Lessthan -> '<'
  | Minus -> '-'
  | Newline -> '\n'
  | Obrace -> '{'
  | Oparenthesis -> '('
  | Obracket -> '['
  | Percent -> '%'
  | Plus -> '+'
  | Question -> '?'
  | Quote -> '\''
  | Semicolon -> ';'
  | Slash -> '/'
  | Space -> ' '
  | Star -> '*'
  | Tab -> assert false
  | Tilde -> '~'
  | Underscore -> '_'

let string_of_token = function
  | Tag (name, o) -> if Utils.debug then "TAG("^name^")" ^ o#to_string else o#to_string
  | Word s | Number s -> s
  | Delim (n, Tab) -> assert (n >= 0); String.make (4*n) ' '
  | Delim (n, d) -> assert (n >= 0); String.make n (char_of_delim d)

let size_and_newlines = function
  | Tag _ -> 0, 0
  | Delim (x, Newline) -> 0, x
  | Delim (x, _) -> x, 0
  | Number s | Word s -> String.length s, 0

let length t =
  let c, nl = size_and_newlines t in
  c + nl

let split_first = function
  | Delim (1, _) | Number _ | Tag _ | Word _ ->
      invalid_arg "Omd_lexer.split_first"
  | Delim (n, d) ->
      assert (n >= 2);
      Delim (1, d), Delim (n-1, d)

let lex_lexbuf lexbuf =
  let result = ref [] in
  try
    while true do
      result := token lexbuf :: !result
    done;
    assert false
  with End_of_file ->
    List.rev !result

let lex s =
  lex_lexbuf (Lexing.from_string s)

type bigstring = (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

let lexbuf_from_bigstring ba =
  let off = ref 0 in
  Lexing.from_function (fun b n ->
    let n = min n (Bigarray.Array1.dim ba - !off) in
    for i = 0 to n - 1 do
      Bytes.set b i (Bigarray.Array1.get ba (!off + i))
    done;
    off := !off + n;
    n
  )

let lex_bigarray ba =
  lex_lexbuf (lexbuf_from_bigstring ba)

let make_space n =
  if n <= 0 then invalid_arg "Omd_lexer.make_space";
  Delim (n, Space)

let string_of_tokens tl =
  let b = Buffer.create 128 in
  List.iter (fun e -> Buffer.add_string b (string_of_token e)) tl;
  Buffer.contents b

let destring_of_tokens ?(limit=max_int) tl =
  let b = Buffer.create 1024 in
  let rec loop (i:int) (tlist:tok list) : unit = match tlist with
    | e::tl ->
        if limit = i then
          loop i []
        else
          begin
            Buffer.add_string b (String.escaped (string_of_token e));
            Buffer.add_string b "::";
            loop (succ i) tl
          end
    | [] ->
        Buffer.add_string b "[]"
  in
  Buffer.contents (loop 0 tl; b)
}
