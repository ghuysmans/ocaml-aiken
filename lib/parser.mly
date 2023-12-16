%token <string> TEXT
%token <char * string> CHOICE
%token <char> ANSWER
%token NL
%token EOF
%start <(string * Answers.t) list> quiz
%start <unit> title
%%
choice: c=CHOICE NL { c }
question: q=TEXT NL l=nonempty_list(choice) a=ANSWER NL { q, Answers.parse l a }
quiz: l=separated_list(NL, question) EOF { l }
title: TEXT EOF { () }
