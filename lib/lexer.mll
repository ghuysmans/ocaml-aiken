{
open Parser
}

let choice = ['A'-'Z']

rule tokenize = parse
| "ANSWER: " (choice as a) { ANSWER a }
| (choice as c) [')' '.'] ' ' ([^'\n']+ as t) { CHOICE (c, t) }
| [^'\n']+ as t { TEXT t }
| '\n' { Lexing.new_line lexbuf; NL }
| eof { EOF }
