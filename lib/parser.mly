%token <string> NAME
%token <int> COUNT
%token SB
%token LF

%start <(bool * int * string) list> deck

%%

deck : line = separated_list(LF, expr); EOF; <>

expr :
  | ~ = br; count = COUNT; name = NAME; { (br, count, name) }

br :
  | BR; { true }
  | { false }
