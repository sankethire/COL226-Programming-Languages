{
    open Parser
}

let int_ = '-'?('0'|['1'-'9']['0'-'9']*)
let add = "+"
let mul = "*"
let lparen = "("
let rparen = ")"
let true_ = "T"
let false_ = "F"
let and_ = "/\\"
let or_ = "\\/"
let if_ = "if"
let then_ = "then"
let else_ = "else"
let fi = "fi"
let cmp = "comp"
let app = "apply"
let lambda = "lambda"
let in = "in"
let id = ['A'-'Z']['a'-'z''A'-'Z''0'-'9''_''\'']*
let whitespace = [' ''\t''\n']
let eq = "="


rule read = parse 
  int_ as n                    {(INT (int_of_string n)) }
  |app                          {(APP)}
  |lambda                       {(LAMBDA)}
  |cmp                          {(CMP)}
  |in                           {(IN)}
  |eq                           {(EQ)}
  |add                          {(ADD) }
  |mul                          {(MUL) }
  |lparen                       {(LP) }
  |rparen                       {(RP) }
  |true_                        {BOOL(true) }
  |false_                       {BOOL(false) }
  |or_                          {(OR) } 
  |and_                         {(AND) }
  |if_                          {(IF) }
  |then_                        {(THEN) }
  |else_                        {(ELSE) }
  |fi                           {(FI)}
  |id as i                      {(ID (i)) }
  |whitespace                   {read lexbuf}             
  |_                            {read lexbuf}
  |eof                          {EOF} 
  