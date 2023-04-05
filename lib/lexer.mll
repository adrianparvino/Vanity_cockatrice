{
	open Parser
}

let digit = ['0'-'9']
let num = (digit | ['1'-'9'] digit*)
let newline = '\n'
let whitespace = [' ' '\t']

rule token = parse
    newline { LF }
  | 'B' 'R' { BR }
