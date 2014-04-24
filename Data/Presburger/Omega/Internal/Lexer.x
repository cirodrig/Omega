-- Lexical analysis for the Haskell fragment that is used by the
-- 'show' functions for expressions, sets, and relations.

{
module Data.Presburger.Omega.Internal.Lexer(alexScanTokens, Token(..))
where
}

%wrapper "basic"

$digit = 0-9
$init  = [a-z_]
$trail = [a-zA-Z0-9\'_]         -- Non-initial letter in identifier
$ntrail = [$white .] # $trail

@int = "-"? $digit+
@identifier = $init $trail*

:-

$white+                         ;

\(                              { const LParenTok }
\)                              { const RParenTok }
\[                              { const LBrackTok }
\]                              { const RBrackTok }
\\                              { const LambdaTok }
\,                              { const CommaTok }
"->"                            { const ToTok }

"nthVariable" / $ntrail         { const Tok_nthVariable }
"intE" / $ntrail                { const Tok_intE }
"varE" / $ntrail                { const Tok_varE }
"notE" / $ntrail                { const Tok_notE }
"disjE" / $ntrail               { const Tok_disjE }
"forallE" / $ntrail             { const Tok_forallE }
"existsE" / $ntrail             { const Tok_existsE }
"trueE" / $ntrail               { const Tok_trueE }
"falseE" / $ntrail              { const Tok_falseE }
"isZeroE" / $ntrail             { const Tok_isZeroE }
"setFromExp" / $ntrail          { const Tok_setFromExp }
"relFromExp" / $ntrail          { const Tok_relFromExp }
"*|"                            { const IntTimesTok }
"|*|"                           { const TimesTok }
"|+|"                           { const PlusTok }
"|-|"                           { const MinusTok }
"|&&|"                          { const AndTok }
"|<=|"                          { const LETok }
"|<|"                           { const LTTok }
"|>=|"                          { const GETok }
"|>|"                           { const GTTok }
"|==|"                          { const EQTok }

@int                            { IntTok . read }
@identifier                     { IdentTok }

{

data Token =
    IntTok !Int
  | IdentTok String
  | LParenTok | RParenTok
  | LBrackTok | RBrackTok
  | LambdaTok
  | CommaTok
  | ToTok
  | Tok_nthVariable | Tok_intE | Tok_varE | Tok_notE | Tok_disjE
  | Tok_forallE | Tok_existsE | Tok_trueE | Tok_falseE | Tok_isZeroE
  | Tok_setFromExp | Tok_relFromExp
  | IntTimesTok | TimesTok | PlusTok | MinusTok | AndTok
  | LETok | LTTok | GETok | GTTok | EQTok
  deriving(Show)

}