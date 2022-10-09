open System 

type DSp ()                                                     =
    inherit 
        System.Collections
              .Generic
              .Dictionary<string, string>()

type ALL ()                                                     =
    inherit System.Collections
                  .Generic
                  .Dictionary<string, FUN>()

and  FUN                                                        =
    {
        /// Is a function
        mutable I : bool
        /// Function's name
        mutable N : string
        /// Function itself   
        mutable F : string->string
        /// Parameter list
        mutable P : string list
        /// Return expression
        mutable R : string
        /// Mother function
        mutable M : ALL                 
    }
    
let mutable ALL : FUN                                           =
    {
        I = true
        N = "ALL"
        F = (fun _ -> "")
        P = []
        R = ""
        M = ALL ()
    }

/// Data space for universal values.
let mutable DATA : DSp list                                     = 

    [ new DSp () ]

module CORE                                                     = 

    /// Checks if a variable has already decleared in a scope.
    let BE (name:string) (scope:int) : bool = 
        DATA.[scope].ContainsKey(name)

    let BEX (id:string) (func:FUN) : bool =
        func.M.ContainsKey(id)
        
    /// Is a literal a name of a value ?
    let IsVar (name:string) : bool =
    
        System.Char.IsLetter(name.[0]) 
        && not (name.Contains("["))
        && not (name.Contains("("))
        && not (name.Contains("{"))
        && not (name.Contains("]"))
        && not (name.Contains(")"))
        && not (name.Contains("}"))

    /// 
    let IsVarX (name:string) (func:FUN) : bool =

        true 

    /// Searchs and returns a value by name from the bottom scope to the highest one.
    let SCAN (name:string) : string = 
        
        let mutable x = DATA.Length - 1
        let mutable result = ""
        
        match name.ToLower() with
            | "true"  -> "true"
            | "false" -> "false"
            |  _      ->
                while x >= 0 && result = "" do
            
                    match BE name x with
                        | true -> result <- DATA.[x].Item(name)
                        |  _   -> ()
                    x <- x - 1

                match result with
                    | "" -> name
                    | _  -> result

    /// Gets rank of an array.
    let ArrDim (str:string) : int =

        let INPUT = 
            str
             .Replace(" ","").Replace("\t","")

        let mutable d = 0
        let mutable i = 0

        while INPUT.[i] = '[' do
            d <- d + 1
            i <- i + 1
        d

    /// Checks if an operant has already been declared.
    let CHECK (name:string) : bool =
        
        let mutable i = DATA.Length - 1
        let mutable there = DATA.[i].ContainsKey(name)
        while i >= 0 && (BE name i) |> not do
            
            i <- i + 1
            there <- DATA.[i].ContainsKey(name)
        
        true
open CORE

module MATH                                                     =

    /// Visually shows the 'true' value
    let mutable Y               : bool                          = 
        
        true

    /// Visually shows the 'false' value
    let         N               : bool                          = 
    
        false

    /// Function list
    let mutable fl              : string list                   = 
    
        []

    /// Is a token function at IT ?
    let f (token:string)        : bool                          = 
        
        List.contains token fl

    /// Is a token function at RT ?
    let F (token:FUN)           : bool                          = 

        token.I

    /// Is a token an operand at IT ?
    let v (token:string)        : bool                          = 

        match token with
    
            | "!"   | "&&"  | "||"  | "^^" 
                    | "!&"  | "!|"  | "!^"
    
            | "="   | ">"   | "<" 
            | "<>"  | ">="  | "<="
    
            | "&&&" | "|||" | "^^^"
            | "!&&" | "!||" | "!^^"
            | "<<<" | ">>>" |  "`"  
    
            |  "+"  |  "-"  |  "*"
            |  "/"  |  "%"  |  "^"
            | "//"
    
            |  "("  |  ")"  |  ":"   
    
            |  "_"  |  "~"  |  ";"
            |  "?"  |  "|"  |  "@" 
            | ":>"  |  "->" |  ".." -> false               
    
            | _ when f token
                -> false
            | _    -> true
    /// Is a token an operand at RT ?
    let V (token:FUN)           : bool                          = 
        
        match token.R with
    
            | "!"   | "&&"  | "||"  | "^^" 
                    | "!&"  | "!|"  | "!^"
    
            | "="   | ">"   | "<" 
            | "<>"  | ">="  | "<="
    
            | "&&&" | "|||" | "^^^"
            | "!&&" | "!||" | "!^^"
            | "<<<" | ">>>" |  "`"  
    
            |  "+"  |  "-"  |  "*"
            |  "/"  |  "%"  |  "^"
            | "//"
    
            |  "("  |  ")"  |  ":"   
    
            |  "_"  |  "~"  |  ";"
            |  "?"  |  "|"  |  "@" 
            | ":>"  |  "->" |  ".." -> false               
    
            | _ when F token
                -> false
            | _    -> true

    /// Is a token an operator at IT ?
    let o (token:string)        : bool                          = 
        
        match token with             
            |  ":"                   
                                
            |  "_"      |  "~"                   
            |  "!"      |  "`"                   
                                
            |  "^"                   
                                
            |  "*"      |  "/"      |  "//"     |  "%"                           
            |  "+"      |  "-"                   
                                
            | "<<<"     | ">>>"                  
                                
            | ">"       | "<"                    
            | ">="      | "<="                   
                                
            | "="       | "<>"                   
                                
            | "&&&"     | "!&&"                  
            | "^^^"     | "!^^"                  
            | "|||"     | "!||"                  
                                
            | "&&"      | "!&"                   
            | "^^"      | "!^"                   
            | "||"      | "!|"       

            | "?"       | "|"   

            | ":>"        
            
            | "@"
            | ";"      
            | ".." 
            | "->"     ->   true     
                               
            |   _      ->   false  
    /// Checks if a token an operator at RT
    let O (token:FUN)           : bool                          = 
    
        match token.R with            
            |  ":"                    
                                
            |  "_"                    
            |  "~"                    
            |  "!"                    
            |  "`"                    
                                
            |  "^"                    
                                
            |  "*"                    
            |  "/"                    
            |  "//"                   
            |  "%"                    
                                
            |  "+"                    
            |  "-"                    
                                
            | "<<<"                  
            | ">>>"                  
                                
            | ">"                    
            | "<"                    
            | ">="                   
            | "<="                   
                                
            | "="                    
            | "<>"                   
                                
            | "&&&"                  
            | "!&&"                  
                                
            | "^^^"                  
            | "!^^"                  
                                
            | "|||"                  
            | "!||"                  
                                
            | "&&"                   
            | "!&"                   
                                
            | "^^"                   
            | "!^"                   
                                
            | "||"                   
            | "!|"     ->   true      

            | "?"      ->   true        
            | "|"      ->   true     

            | ":>"     ->   true      
            
            | "@"
            | ";"      
            | ".." 
            | "->"     ->   true      
                               
            |   _      ->   false

    /// Returns a token's precedence at IT
    let p (token:string)        : int8                          = 
    
        match token with
            |  ":"     ->   13y       
            
            |  "_"     ->   12y       
            |  "~"     ->   12y       
            |  "!"     ->   12y       
            |  "`"     ->   12y       
                                 
            |  "^"     ->   11y       
                                 
            |  "*"     ->   10y       
            |  "/"     ->   10y       
            | "//"     ->   10y       
            |  "%"     ->   10y       
                                 
            |  "+"     ->   09y       
            |  "-"     ->   09y       
                                 
            | "<<<"    ->   08y       
            | ">>>"    ->   08y       
                                 
            | ">"      ->   07y       
            | "<"      ->   07y       
            | ">="     ->   07y       
            | "<="     ->   07y       
                                 
            | "="      ->   06y       
            | "<>"     ->   06y       
                                 
            | "&&&"    ->   05y       
            | "!&&"    ->   05y       
                                 
            | "^^^"    ->   04y       
            | "!^^"    ->   04y       
                              
            | "|||"    ->   03y       
            | "!||"    ->   03y       
                                 
            | "&&"     ->   02y       
            | "!&"     ->   02y       
                                 
            | "^^"     ->   01y       
            | "!^"     ->   01y       
                                 
            | "||"     ->   00y       
            | "!|"     ->   00y       

            | "?"      ->   -1y       
            | "|"      ->   -1y       
            
            | ":>"     ->   -2y

            | ";"      ->   -3y
            | "@"      ->   -3y

            | ".."     ->   -4y
            | "->"     ->   -5y

            |   _      ->   -9y  
    /// Returns a token's precedence at RT
    let P (token:FUN)           : int8                          = 
    
        match token.R with
            |  ":"     ->   13y       
            
            |  "_"     ->   12y       
            |  "~"     ->   12y       
            |  "!"     ->   12y       
            |  "`"     ->   12y       
                                 
            |  "^"     ->   11y       
                                 
            |  "*"     ->   10y       
            |  "/"     ->   10y       
            | "//"     ->   10y       
            |  "%"     ->   10y       
                                 
            |  "+"     ->   09y       
            |  "-"     ->   09y       
                                 
            | "<<<"    ->   08y       
            | ">>>"    ->   08y       
                                 
            | ">"      ->   07y       
            | "<"      ->   07y       
            | ">="     ->   07y       
            | "<="     ->   07y       
                                 
            | "="      ->   06y       
            | "<>"     ->   06y       
                                 
            | "&&&"    ->   05y       
            | "!&&"    ->   05y       
                                 
            | "^^^"    ->   04y       
            | "!^^"    ->   04y       
                              
            | "|||"    ->   03y       
            | "!||"    ->   03y       
                                 
            | "&&"     ->   02y       
            | "!&"     ->   02y       
                                 
            | "^^"     ->   01y       
            | "!^"     ->   01y       
                                 
            | "||"     ->   00y       
            | "!|"     ->   00y       

            | "?"      ->   -1y       
            | "|"      ->   -1y       
            
            | ":>"     ->   -2y

            | ";"      ->   -3y
            | "@"      ->   -3y

            | ".."     ->   -4y
            | "->"     ->   -5y

            |   _      ->   -9y  

    /// Is an operator left-associativity at IT ?
    let l (token:string)        : bool                          = 
    
        match token with                       
            |  ":"     ->   true        
                               
            |  "_"             
            |  "~"             
            |  "!"             
            |  "`"             
                            
            |  "^"     ->   false       
                            
            |  "*"            
            |  "/"            
            |  "//"           
            |  "%"            
                       
            |  "+"            
            |  "-"            
                               
            | "<<<"            
            | ">>>"            
                         
            | ">"              
            | "<"              
            | ">="             
            | "<="             
                         
            | "="              
            | "<>"             
                       
            | "&&&"            
            | "!&&"            
                       
            | "^^^"            
            | "!^^"            
                       
            | "|||"            
            | "!||"            
                         
            | "&&"             
            | "!&"             
                        
            | "^^"             
            | "!^"             
                         
            | "||"             
            | "!|"             

            | "?"              
            | "|"              

            | ":>"             
            
            | ";"      
            | "@"      

            | ".."     
            | "->"     ->   true

            |   _      ->   false
    /// Is an operator left-associativity at RT ?
    let L (token:FUN)           : bool                          = 
    
        match token.R with  
            |  ":"     ->   true        
                               
            |  "_"            
            |  "~"            
            |  "!"            
            |  "`"            
                            
            |  "^"     ->   false        
                            
            |  "*"             
            |  "/"             
            |  "//"            
            |  "%"             
                       
            |  "+"          
            |  "-"          
                       
            | "<<<"         
            | ">>>"         
                       
            | ">"           
            | "<"           
            | ">="          
            | "<="          
                       
            | "="           
            | "<>"          
                       
            | "&&&"         
            | "!&&"         
                       
            | "^^^"         
            | "!^^"         
                       
            | "|||"         
            | "!||"         
                       
            | "&&"          
            | "!&"          
                       
            | "^^"          
            | "!^"          
                       
            | "||"          
            | "!|"          

            | "?"           
            | "|"           

            | ":>"          
            
            | ";"      
            | "@"      

            | ".."     
            | "->"     ->   true

            |   _      ->   false
     
    /// Is an operator binary at IT ?
    let b (token:string)        : bool                          = 

        match token with                           
            |  ":"     ->   true        
                               
            |  "_"            
            |  "~"            
            |  "!"            
            |  "`"            
                            
            |  "^"     ->   false        
                            
            |  "*"            
            |  "/"            
            |  "//"           
            |  "%"            
                       
            |  "+"        
            |  "-"        
                       
            | "<<<"       
            | ">>>"       
                       
            | ">"         
            | "<"         
            | ">="        
            | "<="        
                       
            | "="         
            | "<>"        
                       
            | "&&&"       
            | "!&&"       
                       
            | "^^^"       
            | "!^^"       
                       
            | "|||"       
            | "!||"       
                       
            | "&&"        
            | "!&"        
                       
            | "^^"        
            | "!^"        
                       
            | "||"        
            | "!|"        

            | "?"         
            | "|"         

            | ":>"        
            
            | ";"      
            | "@"      

            | ".."     
            | "->"     ->   true

            |   _      ->   false
    /// Is an operator binary at RT ?
    let B (token:FUN)           : bool                          = 

        match token.R with
            |  ":"     ->   true     
                        
            |  "_"           
            |  "~"           
            |  "!"           
            |  "`"           
                            
            |  "^"     ->   false    
                            
            |  "*"            
            |  "/"            
            |  "//"           
            |  "%"            
                       
            |  "+"            
            |  "-"            
                         
            | "<<<"           
            | ">>>"           
                         
            | ">"             
            | "<"             
            | ">="            
            | "<="            
                         
            | "="             
            | "<>"            
                       
            | "&&&"           
            | "!&&"           
                       
            | "^^^"           
            | "!^^"           
                       
            | "|||"           
            | "!||"           
                         
            | "&&"            
            | "!&"            
                        
            | "^^"            
            | "!^"            
                         
            | "||"            
            | "!|"            

            | "?"             
            | "|"             

            | ":>"            
            
            | ";"      
            | "@"      

            | ".."     
            | "->"     ->   true

            |   _      ->   false

    /// Visually shows every operator's all information at IT
    let i (token:string)        : (bool*int8*bool*bool)         = 
        
        match token with                       
           //  Operator     IsOP?  Prec.     Left?    Binary?          Descripton        
            |  ":"     ->   Y  ,   13y  ,    Y  ,     Y            //  Member access
            
            |  "_"     ->   Y  ,   12y  ,    N  ,     N            //  Unary minus
            |  "~"     ->   Y  ,   12y  ,    N  ,     N            //  Unary plus
            |  "!"     ->   Y  ,   12y  ,    N  ,     N            //  Logical NOT
            |  "`"     ->   Y  ,   12y  ,    N  ,     N            //  Bitwise NOT
                                           
            |  "^"     ->   Y  ,   11y  ,    N  ,     Y            //  Exponent
                                             
            |  "*"     ->   Y  ,   10y  ,    Y  ,     Y            //  Multiplication
            |  "/"     ->   Y  ,   10y  ,    Y  ,     Y            //  Division
            | "//"     ->   Y  ,   10y  ,    Y  ,     Y            //  Floor division
            |  "%"     ->   Y  ,   10y  ,    Y  ,     Y            //  Modulus
                                             
            |  "+"     ->   Y  ,   09y  ,    Y  ,     Y            //  Binary addition
            |  "-"     ->   Y  ,   09y  ,    Y  ,     Y            //  Binary subtraction
                                                 
            | "<<<"    ->   Y  ,   08y  ,    Y  ,     Y            //  Left shift
            | ">>>"    ->   Y  ,   08y  ,    Y  ,     Y            //  Right shift
                                                           
            | ">"      ->   Y  ,   07y  ,    Y  ,     Y            //  Greater than
            | "<"      ->   Y  ,   07y  ,    Y  ,     Y            //  Less than
            | ">="     ->   Y  ,   07y  ,    Y  ,     Y            //  Not less than
            | "<="     ->   Y  ,   07y  ,    Y  ,     Y            //  Not greater than
                                                
            | "="      ->   Y  ,   06y  ,    Y  ,     Y            //  Equal
            | "<>"     ->   Y  ,   06y  ,    Y  ,     Y            //  Not equal
                                              
            | "&&&"    ->   Y  ,   05y  ,    Y  ,     Y            //  Bitwise AND
            | "!&&"    ->   Y  ,   05y  ,    Y  ,     Y            //  Bitwise NAND
                                              
            | "^^^"    ->   Y  ,   04y  ,    Y  ,     Y            //  Bitwise XOR
            | "!^^"    ->   Y  ,   04y  ,    Y  ,     Y            //  Bitwise XNOR
                                     
            | "|||"    ->   Y  ,   03y  ,    Y  ,     Y            //  Bitwise OR
            | "!||"    ->   Y  ,   03y  ,    Y  ,     Y            //  Bitwise NOR
                                                
            | "&&"     ->   Y  ,   02y  ,    Y  ,     Y            //  Logical AND
            | "!&"     ->   Y  ,   02y  ,    Y  ,     Y            //  Logical NAND
                                               
            | "^^"     ->   Y  ,   01y  ,    Y  ,     Y            //  Logical XOR
            | "!^"     ->   Y  ,   01y  ,    Y  ,     Y            //  Logical XNOR
                                                             
            | "||"     ->   Y  ,   00y  ,    Y  ,     Y            //  Logical OR
            | "!|"     ->   Y  ,   00y  ,    Y  ,     Y            //  Logical NOR

            | "?"      ->   Y  ,   -1y  ,    Y  ,     Y            //  If-Then
            | "|"      ->   Y  ,   -1y  ,    Y  ,     Y            //  Else

            | ":>"     ->   Y  ,   -2y  ,    Y  ,     Y            //  Generator

            | ".."     ->   Y  ,   -3y  ,    Y  ,     Y            //  Range
            | "->"     ->   Y  ,   -4y  ,    Y  ,     Y            //  Arrow

            | ";"      ->   Y  ,   -5y  ,    Y  ,     Y            //  Member joint 
            | "@"      ->   Y  ,   -5y  ,    Y  ,     Y            //  Dimension joint
            
            |   _      ->   N  ,   -9y  ,    N  ,     N            
    /// Visually shows every operator's all information at RT
    let I (op:FUN)              : (bool*int8*bool*bool)         = 
    
        match op.R with                       
           //  Operator     IsOP?  Prec.     Left?    Binary?          Descripton        
            |  ":"     ->   Y  ,   13y  ,    Y  ,     Y            //  Member access
            
            |  "_"     ->   Y  ,   12y  ,    N  ,     N            //  Unary minus
            |  "~"     ->   Y  ,   12y  ,    N  ,     N            //  Unary plus
            |  "!"     ->   Y  ,   12y  ,    N  ,     N            //  Logical NOT
            |  "`"     ->   Y  ,   12y  ,    N  ,     N            //  Bitwise NOT
                                           
            |  "^"     ->   Y  ,   11y  ,    N  ,     Y            //  Exponent
                                             
            |  "*"     ->   Y  ,   10y  ,    Y  ,     Y            //  Multiplication
            |  "/"     ->   Y  ,   10y  ,    Y  ,     Y            //  Division
            | "//"     ->   Y  ,   10y  ,    Y  ,     Y            //  Floor division
            |  "%"     ->   Y  ,   10y  ,    Y  ,     Y            //  Modulus
                                             
            |  "+"     ->   Y  ,   09y  ,    Y  ,     Y            //  Binary addition
            |  "-"     ->   Y  ,   09y  ,    Y  ,     Y            //  Binary subtraction
                                                 
            | "<<<"    ->   Y  ,   08y  ,    Y  ,     Y            //  Left shift
            | ">>>"    ->   Y  ,   08y  ,    Y  ,     Y            //  Right shift
                                                           
            | ">"      ->   Y  ,   07y  ,    Y  ,     Y            //  Greater than
            | "<"      ->   Y  ,   07y  ,    Y  ,     Y            //  Less than
            | ">="     ->   Y  ,   07y  ,    Y  ,     Y            //  Not less than
            | "<="     ->   Y  ,   07y  ,    Y  ,     Y            //  Not greater than
                                                
            | "="      ->   Y  ,   06y  ,    Y  ,     Y            //  Equal
            | "<>"     ->   Y  ,   06y  ,    Y  ,     Y            //  Not equal
                                              
            | "&&&"    ->   Y  ,   05y  ,    Y  ,     Y            //  Bitwise AND
            | "!&&"    ->   Y  ,   05y  ,    Y  ,     Y            //  Bitwise NAND
                                              
            | "^^^"    ->   Y  ,   04y  ,    Y  ,     Y            //  Bitwise XOR
            | "!^^"    ->   Y  ,   04y  ,    Y  ,     Y            //  Bitwise XNOR
                                     
            | "|||"    ->   Y  ,   03y  ,    Y  ,     Y            //  Bitwise OR
            | "!||"    ->   Y  ,   03y  ,    Y  ,     Y            //  Bitwise NOR
                                                
            | "&&"     ->   Y  ,   02y  ,    Y  ,     Y            //  Logical AND
            | "!&"     ->   Y  ,   02y  ,    Y  ,     Y            //  Logical NAND
                                               
            | "^^"     ->   Y  ,   01y  ,    Y  ,     Y            //  Logical XOR
            | "!^"     ->   Y  ,   01y  ,    Y  ,     Y            //  Logical XNOR
                                                             
            | "||"     ->   Y  ,   00y  ,    Y  ,     Y            //  Logical OR
            | "!|"     ->   Y  ,   00y  ,    Y  ,     Y            //  Logical NOR

            | "?"      ->   Y  ,   -1y  ,    Y  ,     Y            //  If-Then
            | "|"      ->   Y  ,   -1y  ,    Y  ,     Y            //  Else

            | ":>"     ->   Y  ,   -2y  ,    Y  ,     Y            //  Generator

            | ".."     ->   Y  ,   -3y  ,    Y  ,     Y            //  Range
            | "->"     ->   Y  ,   -4y  ,    Y  ,     Y            //  Arrow

            | ";"      ->   Y  ,   -5y  ,    Y  ,     Y            //  Member joint 
            | "@"      ->   Y  ,   -5y  ,    Y  ,     Y            //  Dimension joint
            
            |   _      ->   N  ,   -9y  ,    N  ,     N     

    /// Extracts and separates all operands and operators in an expression at IT
    let d (input:string)        : string[]                      = 
                
        let mutable rslt = ""
        let lit = [ ' ' .. '~' ] |> List.filter 
                   ( fun c -> System.Char.IsLetterOrDigit(c) )

        let mutable r = false
        let some = input.Replace("\\\"","\u4567")
                        .Replace("\\n" ,"\u4568")
                        .Replace("\\t" ,"\u4569")                        
                        .ToCharArray()
   
        for c in some do
            
            match c = '"' with
                | true -> rslt <- rslt + "\u2923"; r <- not r
                | _    -> 
                    match c with
                        | ' '  -> match r with
                                    | true -> rslt <- rslt + "\u0993"
                                    | _    -> rslt <- rslt + " "
                        | '\t' -> match r with
                                    | true -> rslt <- rslt + "\u0994"
                                    | _    -> ()
                        | ':' -> match r with
                                    | true -> rslt <- rslt + "\u0995"
                                    | _    -> rslt <- rslt + " : "
                        | '.' -> match r with
                                    | true -> rslt <- rslt + "\u0996"
                                    | _    -> rslt <- rslt + "."

                        | ';' -> match r with
                                    | true -> rslt <- rslt + "\u0997"
                                    | _    -> rslt <- rslt + " ; "
                        |  _ -> 

                            match r with
                                | false -> //  I am here.
                                    match List.contains c ('.'::lit) with
                                        | true  -> rslt <- rslt + sprintf  "%c"  c
                                        |  _ when c = '[' 
                                                -> rslt <- rslt + sprintf  " ( %c @ "  c 
                                        |  _ when c = ']' 
                                                -> rslt <- rslt + sprintf  " @ %c ) "  c
    
                                        |  _    -> rslt <- rslt + sprintf  " %c " c

                                | _     -> rslt <- rslt + sprintf  "%c"  c

        while rslt.Contains ("  ") do rslt <- rslt.Replace("  "," ")
        while rslt.Contains (" ~ ") do rslt <- rslt.Replace(" ~ "," ` ")

        let mutable temp =
            rslt
                .Replace( "  "      , " "           )
                .Replace( " & & & " , " &&& "       )
                .Replace( " | | | " , " ||| "       )
                .Replace( " ^ ^ ^ " , " ^^^ "       )
                .Replace( " ! & & " , " !&& "       )
                .Replace( " ! | | " , " !|| "       )
                .Replace( " ! ^ ^ " , " !^^ "       )
                .Replace( " > > > " , " >>> "       )
                .Replace( " < < < " , " <<< "       )
                .Replace( " & & "   , " && "        )
                .Replace( " | | "   , " || "        )
                .Replace( " ! & "   , " !& "        )
                .Replace( " ! | "   , " !| "        )
                .Replace( " ^ ^ "   , " ^^ "        )
                .Replace( " ! ^ "   , " !^ "        )
                .Replace( " < > "   , " <> "        )
                .Replace( " < = "   , " <= "        )
                .Replace( " > = "   , " >= "        )
                .Replace( " / / "   , " // "        )
                .Replace( " : > "   , " :> "        )
                .Replace( " @ @ "   , " @ null @ "  )
                .Replace( " - > "   , " -> "        )
                .Replace( " = > "   , " => ( "      )
                .Replace( ".."      , " `` "        )
                .Replace( "``"      , " .. "        )
                .Replace( "  "      , " "           )
    
        while  temp.Contains(" + + ") || temp.Contains(" - - ") || 
               temp.Contains(" + - ") || temp.Contains(" - + ") do 
                
                temp <- temp.Replace(" + + "   , " + "   )
                            .Replace(" - - "   , " + "   ) 
                            .Replace(" - + "   , " - "   )
                            .Replace(" + - "   , " - "   )
    
        let infix = // I am here !!!
            temp.Split(' ') 
            |> Array.filter (fun s -> s <> "")
            
        match infix.[0] with  // Why here ?  
            | "-" -> infix.[0] <- "_"
            | "+" -> infix.[0] <- "~"
            |  _  ->       ( )
        
        for i in 1 .. infix.Length - 1 do
            
            match infix.[i] with
    
                | "+" -> 
                    match infix.[i-1] = "(" || o infix.[i-1] with
                        | true -> infix.[i] <- "~"
                        |  _   -> ()
                | "-" -> 
                    match infix.[i-1] = "(" || o infix.[i-1] with
                        | true -> infix.[i] <- "_"
                        |  _   -> ()
                |  _ ->  ()
    
    
        infix 
        |> Array.map (fun s -> match s.Length > 3 with
                                | true -> match s.[0..1] with
                                            | "0x" -> System.Convert.ToInt32(s.[2..],16) |> string
                                            | "0o" -> System.Convert.ToInt32(s.[2..],08) |> string
                                            | "0b" -> System.Convert.ToInt32(s.[2..],02) |> string
                                            | _ -> s
                                | _    -> s)
   
    /// Does binary operation at IT
    let uEVAL (lx:string) (op:string) (rx:string)   : string    =
    
        match op with 
            | "!"   | "&&"  | "||"  | "^^"
            | "!!"  | "!&"  | "!|"  | "!^" 
                ->
                let LX = 
                    match lx with
                        | "0" -> false
                        |  _  when lx.ToLower() = "true" 
                                || lx.ToLower() = "false"
                                    -> System.Convert.ToBoolean(lx)
                        |  _  -> true

                let RX = 
                    match rx with
                        | "0" -> false
                        |  _  when rx.ToLower() = "true" 
                                || rx.ToLower() = "false"
                                    -> System.Convert.ToBoolean(rx)
                        |  _  -> true
        
                let ANS =
                    match op with
                        | "!!" -> (RX)                                  // ID
                        | "!"  -> (RX)                           |> not // NOT
                        | "&&" -> (LX && RX)                            // AND 
                        | "!&" -> (LX && RX)                     |> not // NAND
                        | "||" -> (LX || RX)                            // OR
                        | "!|" -> (LX || RX)                     |> not // NOR
                        | "^^" -> (LX && not RX || not LX && RX)        // XOR
                        | "!^" -> (LX && not RX || not LX && RX) |> not // XNOR
                        |  _   ->  true
        
                match ANS with
                    | true -> "true"
                    |  _   -> "false"

            | "="   | ">"   | "<"   
            | "<>"  | ">="  | "<=" 
                ->
                
                match lx.[0], lx.[lx.Length-1],
                      rx.[0], rx.[rx.Length-1] with
                      | '\u2923' , '\u2923',
                        '\u2923' , '\u2923' -> 
                        
                        match op with
                            | "="  -> (sprintf "%b" (lx =  rx)).ToLower()
                            | "<>" -> (sprintf "%b" (lx <> rx)).ToLower()
                            |  _   -> ""

                      | _ when lx.Contains("infinity") ||
                               rx.Contains("infinity") -> 
                                
                            let LX = match lx with
                                        | "-infinity" -> -8./0.
                                        | "+infinity" -> +8./0.
                                        |  _          -> float lx
                            let RX = match rx with
                                        | "-infinity" -> -8./0.
                                        | "+infinity" -> +8./0.
                                        |  _          -> float lx

                            let ANS =
                                match op with
                                    | "="  ->  LX =  RX
                                    | ">"  ->  LX >  RX
                                    | "<"  ->  LX <  RX
                                    | "<>" ->  LX <> RX
                                    | ">=" ->  LX >= RX
                                    | "<=" ->  LX <= RX
                                    |  _   ->  true
        
                            match ANS with
                                | true -> "true"
                                |  _   -> "false"

           
                      | _ ->
                        let LX = 
                            match lx with
                                | "true"  | "True"  -> 1.
                                | "false" | "False" -> 0.
                                |  _ -> System.Convert.ToDouble(lx)
                
                        let RX = 
                            match rx with
                                | "true"  | "True"  -> 1.
                                | "false" | "False" -> 0.
                                |  _ -> System.Convert.ToDouble(rx)
        
                        let ANS =
                            match op with
                                | "="  ->  LX =  RX
                                | ">"  ->  LX >  RX
                                | "<"  ->  LX <  RX
                                | "<>" ->  LX <> RX
                                | ">=" ->  LX >= RX
                                | "<=" ->  LX <= RX
                                |  _   ->  true
        
                        match ANS with
                            | true -> "true"
                            |  _   -> "false"
    
            | "&&&" | "|||" | "^^^" 
            | "!&&" | "!||" | "!^^" 
            | "`"   | "<<<" | ">>>" 
                ->
                   
                let LX = 
                    match lx with
                        | _ when lx.ToLower() = "true"  -> 1
                        | _ when lx.ToLower() = "false" -> 0
                        | _ -> System.Convert.ToDouble(lx) |> int

                let RX = 
                    match rx with
                        | _ when rx.ToLower() = "true"  -> 1
                        | _ when rx.ToLower() = "false" -> 0
                        | _ -> System.Convert.ToDouble(rx) |> int
        
                let ANS =
                    match op with
                        | "`"   ->  ~~~  RX
                        | "&&&" ->      (LX &&& RX)
                        | "!&&" ->  ~~~ (LX &&& RX)
                        | "|||" ->      (LX ||| RX)
                        | "!||" ->  ~~~ (LX ||| RX)
                        | "^^^" ->      (LX ^^^ RX)
                        | "!^^" ->  ~~~ (LX ^^^ RX)
                        | "<<<" ->      (LX <<< RX)
                        | ">>>" ->      (LX >>> RX)
                        |  _   ->            0
        
                ANS.ToString()
    
            | "?"  -> lx + "\u1234" + rx
            | "|"  -> 
                
                let a = lx.Split('\u1234').[0]
                let b = lx.Split('\u1234').[1]
                match a with
                    | "true"  -> b
                    |  _      -> rx

            | "@"  -> 
                  match lx.[0], lx.[lx.Length-1], // !!
                        rx.[0], rx.[rx.Length-1] with
                          | '\u2923' , '\u2923',
                            '\u2923' , '\u2923' -> lx.[0..lx.Length-2] + 
                                                   rx.[1..rx.Length-1]
                          | _    -> 
                
                                match rx with
                                    | "null" -> lx 
                                    | _      -> lx + rx

            | ";"  -> lx + ";" + rx
            
            | ":"  -> 
                
                match lx.[0] , lx.[lx.Length-1] with   
                    | '[' , ']' -> 
                        match rx.[0] , rx.[rx.Length-1] with   
                            | '[' , ']' -> 
                                
                                match rx.Contains("..") with
                                    | false ->

                                        let ArrDim (str:string) : int =

                                            let INPUT = str.Replace(" ","")
                                                           .Replace("\t","")

                                            let mutable d = 0
                                            let mutable i = 0

                                            while INPUT.[i] = '[' do
                                                d <- d + 1
                                                i <- i + 1
                                            d

                                        let result = match ArrDim lx = 1 with
                                                        | true -> lx.[1..lx.Length-2]
                                                                    .Replace(";","\u0666")
                                                                    .Split('\u0666')
                                                        |  _   -> 
                                                                   let L = System.String.Join("",[|for _ in 1 .. -1 + ArrDim lx -> "[" |])
                                                                   let R = System.String.Join("",[|for _ in 1 .. -1 + ArrDim lx -> "]" |])
                                                                   lx.[1..lx.Length-2]
                                                                    .Replace(R+";"+L,R+"\u0666"+L)
                                                                    .Split('\u0666')
                               
                                        result.[rx.[1..rx.Length-2] |> int]
                                    | _ -> 
                                    
                                        let ArrDim (str:string) : int =

                                            let INPUT = str.Replace(" ","")
                                                           .Replace("\t","")

                                            let mutable d = 0
                                            let mutable i = 0

                                            while INPUT.[i] = '[' do
                                                d <- d + 1
                                                i <- i + 1
                                            d

                                        let result = match ArrDim lx = 1 with
                                                        | true -> lx.[1..lx.Length-2]
                                                                    .Replace(";","\u0666")
                                                                    .Split('\u0666')
                                                        |  _   -> 
                                                                   let L = System.String.Join("",[|for _ in 1 .. -1 + ArrDim lx -> "[" |])
                                                                   let R = System.String.Join("",[|for _ in 1 .. -1 + ArrDim lx -> "]" |])
                                                                   lx.[1..lx.Length-2]
                                                                    .Replace(R+";"+L,R+"\u0666"+L)
                                                                    .Split('\u0666')
                               
                                        let wall = rx.[1..rx.Length-2].Replace("..","_").Split('_')
                
                                        let head = wall.[0] |> float |> int
                                        let tail = wall.[wall.Length-1] |> float |> int
                                        let step = match wall.Length with
                                                    | 2 -> match head <= tail with
                                                            | true ->  1. |> int
                                                            |  _   -> -1. |> int
                                                    | 3 -> wall.[1] |> float |> int                                     
                                                    | _ -> wall.[1] |> float |> int 
                        
                                        "[" + System.String.Join(";", [|for i in head .. step .. tail -> result.[i] |]) + "]"
                         
                            |  _ -> 
                                match rx with
                                    | "Length" -> 
                                                  let result = match ArrDim lx = 1 with
                                                                  | true -> lx.[1..lx.Length-2]
                                                                              .Replace(";","\u0666")
                                                                              .Split('\u0666')
                                                                  |  _   -> 
                                                                             let L = System.String.Join("",[|for _ in 1 .. -1 + ArrDim lx -> "[" |])
                                                                             let R = System.String.Join("",[|for _ in 1 .. -1 + ArrDim lx -> "]" |])
                                                                             lx.[1..lx.Length-2]
                                                                              .Replace(R+";"+L,R+"\u0666"+L)
                                                                              .Split('\u0666')
                                                  match result with
                                                    | [|""|] -> "0"
                                                    | _  -> result.Length.ToString()

                                    | "Rank"   -> (ArrDim lx).ToString()
                                    
                                    | "Max"    -> let result = match ArrDim lx = 1 with
                                                                | true -> lx.[1..lx.Length-2]
                                                                            .Replace(";","\u0666")
                                                                            .Split('\u0666')
                                                                |  _   -> 
                                                                          let L = System.String.Join("",[|for _ in 1 .. -1 + ArrDim lx -> "[" |])
                                                                          let R = System.String.Join("",[|for _ in 1 .. -1 + ArrDim lx -> "]" |])
                                                                          lx.[1..lx.Length-2]
                                                                           .Replace(R+";"+L,R+"\u0666"+L)
                                                                           .Split('\u0666')
                                                                
                                                  result |> Array.map (fun s -> float s) 
                                                         |> Array.max 
                                                         |> (fun f -> f.ToString())
                                                         |> sprintf "%s"

                                    | "Min"    -> let result = match ArrDim lx = 1 with
                                                                | true -> lx.[1..lx.Length-2]
                                                                            .Replace(";","\u0666")
                                                                            .Split('\u0666')
                                                                |  _   -> 
                                                                          let L = System.String.Join("",[|for _ in 1 .. -1 + ArrDim lx -> "[" |])
                                                                          let R = System.String.Join("",[|for _ in 1 .. -1 + ArrDim lx -> "]" |])
                                                                          lx.[1..lx.Length-2]
                                                                           .Replace(R+";"+L,R+"\u0666"+L)
                                                                           .Split('\u0666')
                                                                
                                                  result |> Array.map (fun s -> float s) 
                                                         |> Array.min 
                                                         |> (fun f -> f.ToString())
                                                         |> sprintf "%s"
                                    
                                    | "Average"-> let result = match ArrDim lx = 1 with
                                                                | true -> lx.[1..lx.Length-2]
                                                                            .Replace(";","\u0666")
                                                                            .Split('\u0666')
                                                                |  _   -> 
                                                                          let L = System.String.Join("",[|for _ in 1 .. -1 + ArrDim lx -> "[" |])
                                                                          let R = System.String.Join("",[|for _ in 1 .. -1 + ArrDim lx -> "]" |])
                                                                          lx.[1..lx.Length-2]
                                                                           .Replace(R+";"+L,R+"\u0666"+L)
                                                                           .Split('\u0666')
                                                                
                                                  result |> Array.map (fun s -> float s) 
                                                         |> Array.average 
                                                         |> (fun f -> f.ToString())
                                                         |> sprintf "%s"

                                    | "Sort"   -> let result = match ArrDim lx = 1 with
                                                                | true -> lx.[1..lx.Length-2]
                                                                            .Replace(";","\u0666")
                                                                            .Split('\u0666')
                                                                |  _   -> 
                                                                          let L = System.String.Join("",[|for _ in 1 .. -1 + ArrDim lx -> "[" |])
                                                                          let R = System.String.Join("",[|for _ in 1 .. -1 + ArrDim lx -> "]" |])
                                                                          lx.[1..lx.Length-2]
                                                                           .Replace(R+";"+L,R+"\u0666"+L)
                                                                           .Split('\u0666')
                                                                
                                                  result |> Array.map (fun s -> float s) 
                                                         |> Array.sort
                                                         |> (fun a -> System.String.Join(";",a)) 
                                                         |> sprintf "[%s]"
                                    
                                    | "Reverse"-> let result = match ArrDim lx = 1 with
                                                                | true -> lx.[1..lx.Length-2]
                                                                            .Replace(";","\u0666")
                                                                            .Split('\u0666')
                                                                |  _   -> 
                                                                          let L = System.String.Join("",[|for _ in 1 .. -1 + ArrDim lx -> "[" |])
                                                                          let R = System.String.Join("",[|for _ in 1 .. -1 + ArrDim lx -> "]" |])
                                                                          lx.[1..lx.Length-2]
                                                                           .Replace(R+";"+L,R+"\u0666"+L)
                                                                           .Split('\u0666')
                                                                
                                                  result |> Array.rev 
                                                  |> (fun a -> System.String.Join(";",a)) 
                                                  |> sprintf "[%s]"

                                    | _        -> ""
                                
                    | '\u2923' , '\u2923' ->
                        match rx.[0] , rx.[rx.Length-1] with
                            | '[' , ']' -> 
                                match rx.Contains("..") with
                                    | false -> 
                                
                                        let str = lx.[1..lx.Length-2]
                                        let cell = rx.[1..rx.Length-2] |> int
                                        sprintf "%c" str.[cell]
                                    | true ->

                                        let str = lx.ToCharArray()
                                        let result = str.[1..str.Length-2]
                                        let wall = rx.[1..rx.Length-2].Replace("..","_").Split('_')
                
                                        let head = wall.[0] |> float |> int
                                        let tail = wall.[wall.Length-1] |> float |> int
                                        let step = match wall.Length with
                                                    | 2 -> match head <= tail with
                                                            | true ->  1. |> int
                                                            |  _   -> -1. |> int
                                                    | 3 -> wall.[1] |> float |> int                                     
                                                    | _ -> wall.[1] |> float |> int 
                        
                                        "\u2923"+System.String.Join("",[|for i in head..step..tail->result.[i]|])+"\u2923"

                            | _ -> let str = lx.[1..lx.Length-2]
                                   match rx with
                                        | "Length"      -> sprintf "%d" str.Length
                                        | "ToUpper"     -> "\u2923" + str.ToUpper() + "\u2923"  
                                        | "ToLower"     -> "\u2923" + str.ToLower() + "\u2923"
                                        | "Reverse"     -> str.ToCharArray() 
                                                           |> Array.rev
                                                           |> fun arr -> System.String.Join("", arr)
                             
                                        | _             -> ""
                    | _ -> ""

            | ":>" -> 
                System
                 .String
                 .Join(";", [|for _ in 1..rx|>float|>int->lx|])
        
            | ".." -> lx + ".." + rx 

            | "->" ->  match lx.Contains("..") with
                            | true -> 
                                let wall = lx.Replace("..","_").Split('_')
                
                                let head = wall.[0] |> float
                                let tail = wall.[wall.Length-1] |> float
                                let step = match wall.Length with
                                            | 2 -> match head >= tail with
                                                    | true ->  1.
                                                    |  _   -> -1.
                                            | 3 -> wall.[1] |> float                                     
                                            | _ -> 0.
                        
                                System.String.Join(";", [|for _ in head .. step .. tail -> rx |])

                            |  _  when lx.Contains("{") -> ""
                            |  _  -> ""
              
            | "+"  | "-"  | "*"  | "/"   
            | "%"  | "~"  | "**" | "//" 
            | "_"  | "^" ->
                
                let LX = 
                    match lx.ToLower() with
                        | "+infinity" -> +8./0.
                        | "-infinity" -> -8./0.
                        | "true"  -> 1.
                        | "false" -> 0.
                        |    _    -> System.Convert.ToDouble(lx)
                let RX = 
                    match rx.ToLower() with
                        | "+infinity" -> +8./0.
                        | "-infinity" -> -8./0.
                        | "true"  -> 1.
                        | "false" -> 0.
                        |    _    -> System.Convert.ToDouble(rx)
        
                let ANS =
                    match op with
                        | "+"  -> (LX +  RX)
                        | "-"  -> (LX -  RX)
                        | "*"  -> (LX *  RX)
                        | "/"  -> (LX /  RX)
                        | "//" -> (LX /  RX) |> int |> float
                        | "^"  -> (LX ** RX)
                        | "%"  -> 
                            match RX with
                                |  0. -> 0./0.
                                |  _  -> (int LX) % (int RX) |> float
                        | "~"  ->   RX
                        | "_"  -> - RX
                        |  _   ->  0.
        
                match ANS.ToString() with
                    |  "∞" -> "+infinity"
                    | "-∞" -> "-infinity"
                    |  _   -> ANS.ToString()
            
            // Functions
            |  _  -> 
                
                let mutable ans = ""

                ans
    /// Does binary operation at RT
    let uEVALX (lx:FUN) (op:FUN) (rx:FUN)           : string    =
        
        let str =
            match op.I with
                | false ->
                    match op.R with 
                        // Boolean operators
                        | "!"   | "&&"  | "||"  | "^^"
                        | "!!"  | "!&"  | "!|"  | "!^" 
                            ->
                            let LX = 
                                match lx.R with
                                    | "0" -> false
                                    |  _  when lx.R.ToLower() = "true" 
                                            || lx.R.ToLower() = "false"
                                                -> System.Convert.ToBoolean(lx)
                                    |  _  -> true
    
                            let RX = 
                                match rx.R with
                                    | "0" -> false
                                    |  _  when rx.R.ToLower() = "true" 
                                            || rx.R.ToLower() = "false"
                                                -> System.Convert.ToBoolean(rx)
                                    |  _  -> true
            
                            let ANS =
                                match op.R with
                                    | "!!" -> (RX)                                  // ID
                                    | "!"  -> (RX)                           |> not // NOT
                                    | "&&" -> (LX && RX)                            // AND 
                                    | "!&" -> (LX && RX)                     |> not // NAND
                                    | "||" -> (LX || RX)                            // OR
                                    | "!|" -> (LX || RX)                     |> not // NOR
                                    | "^^" -> (LX && not RX || not LX && RX)        // XOR
                                    | "!^" -> (LX && not RX || not LX && RX) |> not // XNOR
                                    |  _   ->  true
            
                            match ANS with
                                | true -> "true"
                                |  _   -> "false"
        
                        // Relational operators
                        | "="   | ">"   | "<"   
                        | "<>"  | ">="  | "<=" 
                            ->
                    
                            match lx.R.[0], lx.R.[lx.R.Length-1],
                                  rx.R.[0], rx.R.[rx.R.Length-1] with
                                  | '\u2923' , '\u2923',
                                    '\u2923' , '\u2923' -> 
                            
                                    match op.R with
                                        | "="  -> (sprintf "%b" (lx.R =  rx.R)).ToLower()
                                        | "<>" -> (sprintf "%b" (lx.R <> rx.R)).ToLower()
                                        |  _   -> ""
    
                                  | _ when lx.R.Contains("infinity") ||
                                           rx.R.Contains("infinity") -> 
                                    
                                        let LX = match lx.R with
                                                    | "-infinity" -> -8./0.
                                                    | "+infinity" -> +8./0.
                                                    |  _          -> float (lx.R)
                                        let RX = match rx.R with
                                                    | "-infinity" -> -8./0.
                                                    | "+infinity" -> +8./0.
                                                    |  _          -> float (lx.R)    
                                        let ANS =
                                            match op.R with
                                                | "="  ->  LX =  RX
                                                | ">"  ->  LX >  RX
                                                | "<"  ->  LX <  RX
                                                | "<>" ->  LX <> RX
                                                | ">=" ->  LX >= RX
                                                | "<=" ->  LX <= RX
                                                |  _   ->  true
            
                                        match ANS with
                                            | true -> "true"
                                            |  _   -> "false"
    
               
                                  | _ ->
                                    let LX = 
                                        match lx.R with
                                            | "true"  | "True"  -> 1.
                                            | "false" | "False" -> 0.
                                            |  _ -> System.Convert.ToDouble(lx)
                    
                                    let RX = 
                                        match rx.R with
                                            | "true"  | "True"  -> 1.
                                            | "false" | "False" -> 0.
                                            |  _ -> System.Convert.ToDouble(rx)
            
                                    let ANS =
                                        match op.R with
                                            | "="  ->  LX =  RX
                                            | ">"  ->  LX >  RX
                                            | "<"  ->  LX <  RX
                                            | "<>" ->  LX <> RX
                                            | ">=" ->  LX >= RX
                                            | "<=" ->  LX <= RX
                                            |  _   ->  true
            
                                    match ANS with
                                        | true -> "true"
                                        |  _   -> "false"
        
                        // Bitwise operators
                        | "&&&" | "|||" | "^^^" 
                        | "!&&" | "!||" | "!^^" 
                        | "`"   | "<<<" | ">>>" 
                            ->
                       
                            let LX = 
                                match lx with
                                    | _ when lx.R.ToLower() = "true"  -> 1
                                    | _ when lx.R.ToLower() = "false" -> 0
                                    | _ -> System.Convert.ToDouble(lx) |> int
    
                            let RX = 
                                match rx with
                                    | _ when rx.R.ToLower() = "true"  -> 1
                                    | _ when rx.R.ToLower() = "false" -> 0
                                    | _ -> System.Convert.ToDouble(rx) |> int
            
                            let ANS =
                                match op.R with
                                    | "`" ->  ~~~  RX
                                    | "&&&" ->      (LX &&& RX)
                                    | "!&&" ->  ~~~ (LX &&& RX)
                                    | "|||" ->      (LX ||| RX)
                                    | "!||" ->  ~~~ (LX ||| RX)
                                    | "^^^" ->      (LX ^^^ RX)
                                    | "!^^" ->  ~~~ (LX ^^^ RX)
                                    | "<<<" ->      (LX <<< RX)
                                    | ">>>" ->      (LX >>> RX)
                                    |  _   ->            0
            
                            ANS.ToString()
        
                        // Ternary operators
                        | "?"  -> lx.R + "\u1234" + rx.R
                        | "|"  -> 
                    
                            let a = lx.R.Split('\u1234').[0]
                            let b = lx.R.Split('\u1234').[1]
                            match a with
                                | "true"  -> b
                                |  _      -> rx.R
    
                        // Concatenate operators
                        | "@"  -> 
                              match lx.R.[0], lx.R.[lx.R.Length-1], // !!
                                    rx.R.[0], rx.R.[rx.R.Length-1] with
                                      | '\u2923' , '\u2923',
                                        '\u2923' , '\u2923' -> lx.R.[0..lx.R.Length-2] + 
                                                               rx.R.[1..rx.R.Length-1]
                                      | _    -> 
                    
                                            match rx.R with
                                                | "null" -> lx.R 
                                                | _      -> lx.R + rx.R
    
                        | ";"  -> lx.R + ";" + rx.R
                
                        // The member operator
                        | ":"  -> 
                    
                            match lx.R.[0] , lx.R.[lx.R.Length-1] with   
                                | '[' , ']' -> 
                                    match rx.R.[0] , rx.R.[rx.R.Length-1] with   
                                        | '[' , ']' -> 
                                    
                                            match rx.R.Contains("..") with
                                                | false ->
    
                                                    let ArrDim (str:string) : int =
    
                                                        let INPUT = str.Replace(" ","")
                                                                       .Replace("\t","")
    
                                                        let mutable d = 0
                                                        let mutable i = 0
    
                                                        while INPUT.[i] = '[' do
                                                            d <- d + 1
                                                            i <- i + 1
                                                        d
    
                                                    let result = match ArrDim lx.R = 1 with
                                                                    | true -> lx.R.[1..lx.R.Length-2]
                                                                                .Replace(";","\u0666")
                                                                                .Split('\u0666')
                                                                    |  _   -> 
                                                                               let L = System.String.Join("",[|for _ in 1 .. -1 + ArrDim lx.R -> "[" |])
                                                                               let R = System.String.Join("",[|for _ in 1 .. -1 + ArrDim lx.R -> "]" |])
                                                                               lx.R.[1..lx.R.Length-2]
                                                                                .Replace(R+";"+L,R+"\u0666"+L)
                                                                                .Split('\u0666')
                                   
                                                    result.[rx.R.[1..rx.R.Length-2] |> int]
                                                | _ -> 
                                        
                                                    let ArrDim (str:string) : int =
    
                                                        let INPUT = str.Replace(" ","")
                                                                       .Replace("\t","")
    
                                                        let mutable d = 0
                                                        let mutable i = 0
    
                                                        while INPUT.[i] = '[' do
                                                            d <- d + 1
                                                            i <- i + 1
                                                        d
    
                                                    let result = match ArrDim lx.R = 1 with
                                                                    | true -> lx.R.[1..lx.R.Length-2]
                                                                                .Replace(";","\u0666")
                                                                                .Split('\u0666')
                                                                    |  _   -> 
                                                                               let L = System.String.Join("",[|for _ in 1 .. -1 + ArrDim (lx.R) -> "[" |])
                                                                               let R = System.String.Join("",[|for _ in 1 .. -1 + ArrDim (lx.R) -> "]" |])
                                                                               lx.R.[1..lx.R.Length-2]
                                                                                 .Replace(R+";"+L,R+"\u0666"+L)
                                                                                 .Split('\u0666')
                                   
                                                    let wall = rx.R.[1..rx.R.Length-2].Replace("..","_").Split('_')
                    
                                                    let head = wall.[0] |> float |> int
                                                    let tail = wall.[wall.Length-1] |> float |> int
                                                    let step = match wall.Length with
                                                                | 2 -> match head <= tail with
                                                                        | true ->  1. |> int
                                                                        |  _   -> -1. |> int
                                                                | 3 -> wall.[1] |> float |> int                                     
                                                                | _ -> wall.[1] |> float |> int 
                            
                                                    "[" + System.String.Join(";", [|for i in head .. step .. tail -> result.[i] |]) + "]"
                             
                                        |  _ -> 
                                            match rx.R with
                                                | "Length" -> 
                                                              let result = match ArrDim lx.R = 1 with
                                                                              | true -> lx.R.[1..lx.R.Length-2]
                                                                                          .Replace(";","\u0666")
                                                                                          .Split('\u0666')
                                                                              |  _   -> 
                                                                                         let L = System.String.Join("",[|for _ in 1 .. -1 + ArrDim lx.R -> "[" |])
                                                                                         let R = System.String.Join("",[|for _ in 1 .. -1 + ArrDim lx.R -> "]" |])
                                                                                         lx.R.[1..lx.R.Length-2]
                                                                                           .Replace(R+";"+L,R+"\u0666"+L)
                                                                                           .Split('\u0666')
                                                              match result with
                                                                | [|""|] -> "0"
                                                                | _  -> result.Length.ToString()
    
                                                | "Rank"   -> (ArrDim lx.R).ToString()
                                        
                                                | "Max"    -> let result = match ArrDim lx.R = 1 with
                                                                            | true -> lx.R.[1..lx.R.Length-2]
                                                                                        .Replace(";","\u0666")
                                                                                        .Split('\u0666')
                                                                            |  _   -> 
                                                                                      let L = System.String.Join("",[|for _ in 1 .. -1 + ArrDim lx.R -> "[" |])
                                                                                      let R = System.String.Join("",[|for _ in 1 .. -1 + ArrDim lx.R -> "]" |])
                                                                                      lx.R.[1..lx.R.Length-2]
                                                                                        .Replace(R+";"+L,R+"\u0666"+L)
                                                                                        .Split('\u0666')
                                                                    
                                                              result |> Array.map (fun s -> float s) 
                                                                     |> Array.max 
                                                                     |> (fun f -> f.ToString())
                                                                     |> sprintf "%s"
    
                                                | "Min"    -> let result = match ArrDim lx.R = 1 with
                                                                            | true -> lx.R.[1..lx.R.Length-2]
                                                                                        .Replace(";","\u0666")
                                                                                        .Split('\u0666')
                                                                            |  _   -> 
                                                                                      let L = System.String.Join("",[|for _ in 1 .. -1 + ArrDim lx.R -> "[" |])
                                                                                      let R = System.String.Join("",[|for _ in 1 .. -1 + ArrDim lx.R -> "]" |])
                                                                                      lx.R.[1..lx.R.Length-2]
                                                                                        .Replace(R+";"+L,R+"\u0666"+L)
                                                                                        .Split('\u0666')
                                                                    
                                                              result |> Array.map (fun s -> float s) 
                                                                     |> Array.min 
                                                                     |> (fun f -> f.ToString())
                                                                     |> sprintf "%s"
                                        
                                                | "Average"-> let result = match ArrDim lx.R = 1 with
                                                                            | true -> lx.R.[1..lx.R.Length-2]
                                                                                        .Replace(";","\u0666")
                                                                                        .Split('\u0666')
                                                                            |  _   -> 
                                                                                      let L = System.String.Join("",[|for _ in 1 .. -1 + ArrDim lx.R -> "[" |])
                                                                                      let R = System.String.Join("",[|for _ in 1 .. -1 + ArrDim lx.R -> "]" |])
                                                                                      lx.R.[1..lx.R.Length-2]
                                                                                        .Replace(R+";"+L,R+"\u0666"+L)
                                                                                        .Split('\u0666')
                                                                    
                                                              result |> Array.map (fun s -> float s) 
                                                                     |> Array.average 
                                                                     |> (fun f -> f.ToString())
                                                                     |> sprintf "%s"
    
                                                | "Sort"   -> let result = match ArrDim lx.R = 1 with
                                                                            | true -> lx.R.[1..lx.R.Length-2]
                                                                                        .Replace(";","\u0666")
                                                                                        .Split('\u0666')
                                                                            |  _   -> 
                                                                                      let L = System.String.Join("",[|for _ in 1 .. -1 + ArrDim lx.R -> "[" |])
                                                                                      let R = System.String.Join("",[|for _ in 1 .. -1 + ArrDim lx.R -> "]" |])
                                                                                      lx.R.[1..lx.R.Length-2]
                                                                                        .Replace(R+";"+L,R+"\u0666"+L)
                                                                                        .Split('\u0666')
                                                                     
                                                              result |> Array.map (fun s -> float s) 
                                                                     |> Array.sort
                                                                     |> (fun a -> System.String.Join(";",a)) 
                                                                     |> sprintf "[%s]"
                                        
                                                | "Reverse"-> let result = match ArrDim lx.R = 1 with
                                                                            | true -> lx.R.[1..lx.R.Length-2]
                                                                                        .Replace(";","\u0666")
                                                                                        .Split('\u0666')
                                                                            |  _   -> 
                                                                                      let L = System.String.Join("",[|for _ in 1 .. -1 + ArrDim lx.R -> "[" |])
                                                                                      let R = System.String.Join("",[|for _ in 1 .. -1 + ArrDim lx.R -> "]" |])
                                                                                      lx.R.[1..lx.R.Length-2]
                                                                                        .Replace(R+";"+L,R+"\u0666"+L)
                                                                                        .Split('\u0666')
                                                                    
                                                              result |> Array.rev 
                                                              |> (fun a -> System.String.Join(";",a)) 
                                                              |> sprintf "[%s]"
    
                                                | _        -> ""
                                    
                                | '\u2923' , '\u2923' ->
                                    match rx.R.[0] , rx.R.[rx.R.Length-1] with
                                        | '[' , ']' -> 
                                            match rx.R.Contains("..") with
                                                | false -> 
                                    
                                                    let str  = lx.R.[1..lx.R.Length-2]
                                                    let cell = rx.R.[1..rx.R.Length-2] |> int
                                                    sprintf "%c" str.[cell]

                                                | true ->
    
                                                    let str = lx.R.ToCharArray()
                                                    let result = str.[1..str.Length-2]
                                                    let wall = rx.R.[1..rx.R.Length-2].Replace("..","_").Split('_')
                    
                                                    let head = wall.[0] |> float |> int
                                                    let tail = wall.[wall.Length-1] |> float |> int
                                                    let step = match wall.Length with
                                                                | 2 -> match head <= tail with
                                                                        | true ->  1. |> int
                                                                        |  _   -> -1. |> int
                                                                | 3 -> wall.[1] |> float |> int                                     
                                                                | _ -> wall.[1] |> float |> int 
                            
                                                    "\u2923"+System.String.Join("",[|for i in head..step..tail->result.[i]|])+"\u2923"
    
                                        | _ -> let str = lx.R.[1..lx.R.Length-2]
                                               match rx.R with
                                                    | "Length"      -> sprintf "%d" str.Length
                                                    | "ToUpper"     -> "\u2923" + str.ToUpper() + "\u2923"  
                                                    | "ToLower"     -> "\u2923" + str.ToLower() + "\u2923"
                                                    | "Reverse"     -> str.ToCharArray() 
                                                                       |> Array.rev
                                                                       |> fun arr -> System.String.Join("", arr)
                                 
                                                    | _             -> ""
                                | _ -> ""
    
                        // The generate operator
                        | ":>" -> 
                            System
                             .String
                             .Join(";", [|for _ in 1..rx.R|>float|>int->lx|])
            
                        // The range operator
                        | ".." -> lx.R + ".." + rx.R 
    
                        | "->" ->  match lx.R.Contains("..") with
                                        | true -> 
                                            let wall = lx.R.Replace("..","_").Split('_')
                    
                                            let head = wall.[0] |> float
                                            let tail = wall.[wall.Length-1] |> float
                                            let step = match wall.Length with
                                                        | 2 -> match head >= tail with
                                                                | true ->  1.
                                                                |  _   -> -1.
                                                        | 3 -> wall.[1] |> float                                     
                                                        | _ -> 0.
                            
                                            System.String.Join(";", [|for _ in head .. step .. tail -> rx |])
    
                                        |  _  when lx.R.Contains("{") -> ""
                                        |  _  -> ""
                  
                        // Arithmetic operators
                        | "+"  | "-"  | "*"  | "/"   
                        | "%"  | "~"  | "**" | "//" 
                        | "_"  | "~" ->
                    
                            let LX = 
                                match lx.R.ToLower() with
                                    | "+infinity" -> +8./0.
                                    | "-infinity" -> -8./0.
                                    | "true"  -> 1.
                                    | "false" -> 0.
                                    |    _    -> System.Convert.ToDouble(lx)
                            let RX = 
                                match rx.R.ToLower() with
                                    | "+infinity" -> +8./0.
                                    | "-infinity" -> -8./0.
                                    | "true"  -> 1.
                                    | "false" -> 0.
                                    |    _    -> System.Convert.ToDouble(rx)
            
                            let ANS =
                                match op.R with
                                    | "+"  -> (LX +  RX)
                                    | "-"  -> (LX -  RX)
                                    | "*"  -> (LX *  RX)
                                    | "/"  -> (LX /  RX)
                                    | "//" -> (LX /  RX) |> int |> float
                                    | "^"  -> (LX ** RX)
                                    | "%"  -> 
                                        match RX with
                                            |  0. -> 0./0.
                                            |  _  -> (int LX) % (int RX) |> float
                                    | "~"  ->   RX
                                    | "_"  -> - RX
                                    |  _   ->  0.
            
                            match ANS.ToString() with
                                |  "∞" -> "+infinity"
                                | "-∞" -> "-infinity"
                                |  _   -> ANS.ToString()
                
                        // Functions
                        |  _  -> 
                    
                            let mutable ans = ""
                            ans

                | _ -> (op.F lx.R).ToString()
                  
                   
        str

    /// Processes a postfix expression at IT
    let EVAL (postfix:string[])                     : string    =
        
        let stack = new System.Collections.Generic.Stack<string>()
        let mutable lx : string = ""
        let mutable rx : string = ""
        let mutable ans         = ""
    
        match postfix.Length with
            | 1 -> match postfix.[0] with
                    | "true" | "false" -> ans <- postfix.[0]
                    | _ -> match System.Double.TryParse(postfix.[0]) with
                            | true , _ -> ans <- postfix.[0] |> float |> string
                            | _        -> ans <- postfix.[0]
            | _ ->
                for s in postfix do
    
                    match o s with
                        | true -> 
                            rx <- stack.Pop().ToString()
                            lx <- 
                                match s |> b with 
                                    | true -> stack.Pop().ToString()
                                    |  _   -> 
                                        match s with
                                            | "!" -> "true"
                                            |  _  -> "0"
                            ans <- uEVAL lx s rx
                            stack.Push(ans)
    
                        |  _  -> 
                            stack.Push(s)
        ans
    /// Processes a postfix expression at RT
    let EVALX (postfix:FUN[])                                   =
            
        let stack = new System.Collections.Generic.Stack<FUN>()
        let mutable lx  : FUN = ALL
        let mutable rx  : FUN = ALL
        let mutable ans : FUN = ALL
        
        match postfix.Length with
            | 1 -> match postfix.[0].R.ToLower() with
                    | "true" | "false" -> ans.R <- postfix.[0].R
                    | _ -> match System.Double.TryParse(postfix.[0].R) with
                            | true , _ -> ans.R <- postfix.[0].R |> float |> string
                            | _        -> ans.R <- postfix.[0].R
            | _ ->
                for s in postfix do
        
                    match F s with
                        | true -> 
                            match s.P.Length with
                                | 0 ->
                                    ans.I <- false
                                    ans.P <- lx.P.[1..]
                                    ans.R <- rx.F ""
                                    stack.Push(ans)

                                | _ -> 
                                    rx <- stack.Pop()
                                    ans.F <- (fun _ -> s.F rx.R)
                                    stack.Push(ans)
        
                        |  _  -> 
                            rx.R <- stack.Pop().ToString()
                            lx.R <- 
                                match s |> B with 
                                    | true -> stack.Pop().ToString()
                                    |  _   -> 
                                        match s.R with
                                            | "!" -> "true"
                                            |  _  -> "0"
                            ans.R <- uEVALX lx s rx
                            stack.Push(ans)
                            
                            stack.Push(s)
        ans
    
    /// Converts an infix expression to its postfix form at IT
    let sya (infix:string[]) : string[]                         =
        
        let stack = System.Collections.Generic.Stack<string>()
        let queue = System.Collections.Generic.Queue<string>()
    
        for x in infix do
    
            match x with
                | _ when x |> v  -> queue.Enqueue(x)
                | _ when x |> f -> queue.Enqueue(x) // B4: stack.Push(x)
                | _ when x |> o   -> 
                    while ((stack.Count > 0) && ((p(stack.Peek()) > p(x)) || (p(stack.Peek()) = p(x)) && (l(x)))) && (stack.Peek() <> "(") do 
                        queue.Enqueue(stack.Pop())
                    stack.Push(x)
                | _ when x = "(" -> stack.Push(x)
                | _ when x = ")" -> 
                    while stack.Peek() <> "(" do queue.Enqueue(stack.Pop())
                    if stack.Peek() = "(" then stack.Pop() |> ignore
                | _ -> ()
        
        while stack.Count > 0 do queue.Enqueue(stack.Pop())
    
        queue.ToArray()
    /// Converts an infix expression to its postfix form at RT
    let SYA (infix:FUN[])       : FUN []                        =
        
        let stack = System.Collections.Generic.Stack<FUN>()
        let queue = System.Collections.Generic.Queue<FUN>()
    
        for x in infix do
    
            match x with
                | _ when x |> V  -> queue.Enqueue(x)
                | _ when x |> F  -> queue.Enqueue(x) // B4: stack.Push(x)
                | _ when x |> O  -> 
                    while ((stack.Count > 0) && ((P(stack.Peek()) > P(x)) || (P(stack.Peek()) = P(x)) && (L(x)))) && (stack.Peek().R <> "(") do 
                        queue.Enqueue(stack.Pop())
                    stack.Push(x)
                | _ when x.R = "(" -> stack.Push(x)
                | _ when x.R = ")" -> 
                    while stack.Peek().R <> "(" do queue.Enqueue(stack.Pop())
                    if stack.Peek().R = "(" then stack.Pop() |> ignore
                | _ -> ()
        
        while stack.Count > 0 do queue.Enqueue(stack.Pop())
    
        queue.ToArray()

    /// Replaces identifier by theirs respectively explicit value.
    let A (infix:string[]) : string[] =

        infix
        |> Array.map ( 
            fun token -> 
                //match IsVar v with 
                  match v token with
                    | true -> 
                        match SCAN token with  
                            | "" when token.ToLower() = "true"  -> "true"
                            | "" when token.ToLower() = "false" -> "false"
                            | _ -> SCAN token
                    | _  ->  token )

    /// Computation core of the language.
    let CALC (input:string) : string                            = 
    
        let STDI = d input

        match Array.filter (fun s -> s <> "stdi") STDI = STDI with
            | true -> ()
            | _    -> 
                System.Console.ForegroundColor <- System.ConsoleColor.Cyan
                match DATA.[0].ContainsKey("stdi") with
                    | true -> 
                        DATA.[0].Item("stdi") <- System.Console.ReadLine()|>d|>A|>sya|>EVAL
                    | _    ->
                        DATA.[0].Add("stdi", System.Console.ReadLine()|>d|>A|>sya|>EVAL)

                System.Console.ForegroundColor <- System.ConsoleColor.White

        match Array.filter (fun s -> s <> "stri") STDI = STDI with
            | true -> ()
            | _    -> 
                System.Console.ForegroundColor <- System.ConsoleColor.Cyan
                match DATA.[0].ContainsKey("stri") with
                    | true -> 
                        DATA.[0].Item("stri") <- "\""+System.Console.ReadLine().Replace(" ","\u0993").Replace("\t","\u0994")+"\""//\u2923"
                    | _    ->
                        DATA.[0].Add("stri", "\""+System.Console.ReadLine().Replace(" ","\u0993").Replace("\t","\u0994")+"\"")
                System.Console.ForegroundColor <- System.ConsoleColor.White
        
        STDI |> A |> sya |> EVAL
open MATH    

module ELSE                                                     = 

    ()
open ELSE

let MATH (enable:unit) = 

    let all : string[] = [| "" |]
    while all.[0] <> "END" do
    
        printf "\n  >>  Enter an expression: "
        all.[0] <- () |> System.Console.ReadLine
        printf "\n  ->  %s\n" (all.[0] |> CALC)

let TEST (what:string) : unit =

    match what.ToUpper () with
        | "MATH" -> MATH ()
        |  _     -> ()

TEST "math"

printf "\n  ::  v0.0"
System.Threading.Thread.Sleep 5000