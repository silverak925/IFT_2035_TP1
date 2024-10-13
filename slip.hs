    -- TP-1  --- Implantation d'une sorte de Lisp          -*- coding: utf-8 -*-
    -- Joel Villeneuve - 20218950
    -- Karim El-Hajj-Moussa-Ali - 20131773
    
{-# OPTIONS_GHC -Wall #-}
--
-- Ce fichier défini les fonctionalités suivantes:
-- - Analyseur lexical
-- - Analyseur syntaxique
-- - Pretty printer
-- - Implantation du langage

---------------------------------------------------------------------------
-- Importations de librairies et définitions de fonctions auxiliaires    --
---------------------------------------------------------------------------

import Text.ParserCombinators.Parsec -- Bibliothèque d'analyse syntaxique.
import Data.Char                -- Conversion de Chars de/vers Int et autres.
import System.IO                -- Pour stdout, hPutStr

---------------------------------------------------------------------------
-- La représentation interne des expressions de notre language           --
---------------------------------------------------------------------------
data Sexp = Snil                        -- La liste vide
          | Ssym String                 -- Un symbole
          | Snum Int                    -- Un entier
          | Snode Sexp [Sexp]           -- Une liste non vide
          -- Génère automatiquement un pretty-printer et une fonction de
          -- comparaison structurelle.
          deriving (Show, Eq)

-- Exemples:
-- (+ 2 3) ==> Snode (Ssym "+")
--                   [Snum 2, Snum 3]
--
-- (/ (* (- 68 32) 5) 9)
--     ==>
-- Snode (Ssym "/")
--       [Snode (Ssym "*")
--              [Snode (Ssym "-")
--                     [Snum 68, Snum 32],
--               Snum 5],
--        Snum 9]

---------------------------------------------------------------------------
-- Analyseur lexical                                                     --
---------------------------------------------------------------------------

pChar :: Char -> Parser ()
pChar c = do { _ <- char c; return () }

-- Les commentaires commencent par un point-virgule et se terminent
-- à la fin de la ligne.
pComment :: Parser ()
pComment = do { pChar ';'; _ <- many (satisfy (\c -> not (c == '\n')));
                (pChar '\n' <|> eof); return ()
              }
-- N'importe quelle combinaison d'espaces et de commentaires est considérée
-- comme du blanc.
pSpaces :: Parser ()
pSpaces = do { _ <- many (do { _ <- space ; return () } <|> pComment);
               return () }

-- Un nombre entier est composé de chiffres.
integer     :: Parser Int
integer = do c <- digit
             integer' (digitToInt c)
          <|> do _ <- satisfy (\c -> (c == '-'))
                 n <- integer
                 return (- n)
    where integer' :: Int -> Parser Int
          integer' n = do c <- digit
                          integer' (10 * n + (digitToInt c))
                       <|> return n

-- Les symboles sont constitués de caractères alphanumériques et de signes
-- de ponctuations.
pSymchar :: Parser Char
pSymchar    = alphaNum <|> satisfy (\c -> c `elem` "!@$%^&*_+-=:|/?<>")
pSymbol :: Parser Sexp
pSymbol= do { s <- many1 (pSymchar);
              return (case parse integer "" s of
                        Right n -> Snum n
                        _ -> Ssym s)
            }

---------------------------------------------------------------------------
-- Analyseur syntaxique                                                  --
---------------------------------------------------------------------------

-- La notation "'E" est équivalente à "(quote E)"
pQuote :: Parser Sexp
pQuote = do { pChar '\''; pSpaces; e <- pSexp;
              return (Snode (Ssym "quote") [e]) }

-- Une liste est de la forme:  ( {e} [. e] )
pList :: Parser Sexp
pList  = do { pChar '('; pSpaces;
              ses <- pTail;
                    return (case ses of [] -> Snil
                                        se : ses' -> Snode se ses')
            }
pTail :: Parser [Sexp]
pTail  = do { pChar ')'; return [] }
     -- <|> do { pChar '.'; pSpaces; e <- pSexp; pSpaces;
     --          pChar ')' <|> error ("Missing ')' after: " ++ show e);
     --          return e }
     <|> do { e <- pSexp; pSpaces; es <- pTail; return (e : es) }

-- Accepte n'importe quel caractère: utilisé en cas d'erreur.
pAny :: Parser (Maybe Char)
pAny = do { c <- anyChar ; return (Just c) } <|> return Nothing

-- Une Sexp peut-être une liste, un symbol ou un entier.
pSexpTop :: Parser Sexp
pSexpTop = do { pSpaces;
                pList <|> pQuote <|> pSymbol
                <|> do { x <- pAny;
                         case x of
                           Nothing -> pzero
                           Just c -> error ("Unexpected char '" ++ [c] ++ "'")
                       }
              }

-- On distingue l'analyse syntaxique d'une Sexp principale de celle d'une
-- sous-Sexp: si l'analyse d'une sous-Sexp échoue à EOF, c'est une erreur de
-- syntaxe alors que si l'analyse de la Sexp principale échoue cela peut être
-- tout à fait normal.
pSexp :: Parser Sexp
pSexp = pSexpTop <|> error "Unexpected end of stream"

-- Une séquence de Sexps.
pSexps :: Parser [Sexp]
pSexps = do pSpaces
            many (do e <- pSexpTop
                     pSpaces
                     return e)

-- Déclare que notre analyseur syntaxique peut-être utilisé pour la fonction
-- générique "read".
instance Read Sexp where
    readsPrec _p s = case parse pSexp "" s of
                      Left _ -> []
                      Right e -> [(e,"")]

---------------------------------------------------------------------------
-- Sexp Pretty Printer                                                   --
---------------------------------------------------------------------------

showSexp' :: Sexp -> ShowS
showSexp' Snil = showString "()"
showSexp' (Snum n) = showsPrec 0 n
showSexp' (Ssym s) = showString s
showSexp' (Snode h t) =
    let showTail [] = showChar ')'
        showTail (e : es) =
            showChar ' ' . showSexp' e . showTail es
    in showChar '(' . showSexp' h . showTail t

-- On peut utiliser notre pretty-printer pour la fonction générique "show"
-- (utilisée par la boucle interactive de GHCi).  Mais avant de faire cela,
-- il faut enlever le "deriving Show" dans la déclaration de Sexp.
{-
instance Show Sexp where
    showsPrec p = showSexp'
-}

-- Pour lire et imprimer des Sexp plus facilement dans la boucle interactive
-- de Hugs/GHCi:
readSexp :: String -> Sexp
readSexp = read
showSexp :: Sexp -> String
showSexp e = showSexp' e ""

---------------------------------------------------------------------------
-- Représentation intermédiaire Lexp                                     --
---------------------------------------------------------------------------

type Var = String

data Lexp = Lnum Int             -- Constante entière.
          | Lbool Bool           -- Constante Booléenne.
          | Lvar Var             -- Référence à une variable.
          | Ltest Lexp Lexp Lexp -- Expression conditionelle.
          | Lfob [Var] Lexp      -- Construction de fobjet.
          | Lsend Lexp [Lexp]    -- Appel de fobjet.
          | Llet Var Lexp Lexp   -- Déclaration non-récursive.
          -- Déclaration d'une liste de variables qui peuvent être
          -- mutuellement récursives.
          | Lfix [(Var, Lexp)] Lexp
          deriving (Show, Eq)

-- Première passe simple qui analyse une Sexp et construit une Lexp équivalente.
s2l :: Sexp -> Lexp
s2l (Snum n) = Lnum n
s2l (Ssym s) = Lvar s

-- Analyse des Snodes possibles
s2l (Snode (Ssym "if") [condition, trueBranch, falseBranch]) =
  -- On evalue les deux possibles outcomes
  Ltest (s2l cond) (s2l trueBranch) (s2l falseBranch)

s2l (Snode (Ssym "let") [Ssym var, expr, body]) =
  Llet var (s2l expr) (s2l body)

s2l (Snode (Ssym "lambda") [Snode params, body]) =
  Lfob (map s2lParam params) (s2l body)
  where
    s2lParam (Ssym v) = v
    s2lParam _ = error "Parametre lambda invalide"

s2l (Snode func args) = Lsend (s2l func) (map s2l args)

-- Erreur pour tout autre expression inconnue
s2l _ = error "Expression Sexp inconnue: " ++ show se)"

{-
-- Helper function to extract Int from Lnum
getNum :: Lexp -> Maybe Int
getNum (Lnum n) = Just n
getNum _        = Nothing

s2l (Snode (Ssym "+") listExp) =
    let args = map s2l listExp  -- Applique récursivement `s2l` à chaque sous-expression
    in case sequenceA (map getNum args) of
        Just nums -> Lnum (sum nums)  -- Si tous sont des entiers constants, calcule la somme
        Nothing   -> Lsend (Lvar "+") args  -- Sinon, différer l'évaluation avec `Lsend`

s2l (Snode (Ssym "-") listExp) =
    let args = map s2l listExp
    in case sequenceA (map getNum args) of
        Just (x:xs) -> Lnum (x - sum xs)  -- Pour la soustraction, on soustrait la somme des autres
        Nothing      -> Lsend (Lvar "-") args

s2l (Snode (Ssym "*") listExp) =
    let args = map s2l listExp
    in case sequenceA (map getNum args) of
        Just nums -> Lnum (product nums)  -- Produit des entiers
        Nothing   -> Lsend (Lvar "*") args

s2l (Snode (Ssym "/") listExp) =
    let args = map s2l listExp
    in case sequenceA (map getNum args) of
        Just (x:xs) -> Lnum (foldl (\acc n -> acc `div` n) x xs)  -- Division
        Nothing      -> Lsend (Lvar "/") args

--Prend les boolean en arguments
getBool :: Lexp -> Maybe Bool
getBool (Lbool b) = Just b
getBool _         = Nothing

s2l (Snode (Ssym "and") listExp) =
    let args = map s2l listExp
    in case sequenceA (map getBool args) of
        Just bools -> Lbool (and bools)
        Nothing    -> Lsend (Lvar "and") args

s2l (Snode (Ssym "or") listExp) =
    let args = map s2l listExp
    in case sequenceA (map getBool args) of
        Just bools -> Lbool (or bools)
        Nothing    -> Lsend (Lvar "or") args

s2l (Snode (Ssym "not") [e]) =
    let arg = s2l e
    in case arg of
        Lbool b -> Lbool (not b)
        _       -> Lsend (Lvar "not") [arg]

s2l (Snode (Ssym "let") [Snode [Snode [Ssym var, e1]], e2]) = 
    Llet var (s2l e1) (s2l e2)

s2l (Snode (Ssym "fix") [Snode bindings, body]) =
    Lfix (map (\(Snode [Ssym v, e]) -> (v, s2l e)) bindings) (s2l body)        

s2l (Snode _ _) = error "Expression Sexp inconnue"
s2l Snil = Lsend (Lvar "nil") []  -- Cas pour la liste vide
s2l se = error ("Expression Sexp inconnue: " ++ show se)

-}


---------------------------------------------------------------------------
-- Représentation du contexte d'exécution                                --
---------------------------------------------------------------------------

-- Type des valeurs manipulées à l'exécution.
data Value = Vnum Int
           | Vbool Bool
           | Vbuiltin ([Value] -> Value)
           | Vfob VEnv [Var] Lexp

instance Show Value where
    showsPrec p (Vnum n) = showsPrec p n
    showsPrec p (Vbool b) = showsPrec p b
    showsPrec _ (Vbuiltin _) = showString "<primitive>"
    showsPrec _ (Vfob _ _ _) = showString "<fobjet>"

type VEnv = [(Var, Value)]

-- L'environnement initial qui contient les fonctions prédéfinies.
env0 :: VEnv
env0 = let binop f op =
              Vbuiltin (\vs -> case vs of
                         [Vnum n1, Vnum n2] -> f (n1 `op` n2)
                         [_, _] -> error "Pas un nombre"
                         _ -> error "Nombre d'arguments incorrect")

          in [("+", binop Vnum (+)),
              ("*", binop Vnum (*)),
              ("/", binop Vnum div),
              ("-", binop Vnum (-)),
              ("<", binop Vbool (<)),
              (">", binop Vbool (>)),
              ("≤", binop Vbool (<=)),
              ("≥", binop Vbool (>=)),
              ("=", binop Vbool (==)),
              ("true",  Vbool True),
              ("false", Vbool False)]

---------------------------------------------------------------------------
-- Évaluateur                                                            --
---------------------------------------------------------------------------

eval :: VEnv -> Lexp -> Value
-- ¡¡ COMPLETER !!
eval _ (Lnum n) = Vnum n

eval env (Lvar v) =
  case lookup v env of
    Just val -> val
    Nothing -> error ("Variable non définie: " ++ v)
eval env (Ltest cond trueBranch falseBranch) =
  case eval env cond of
    Vbool True  -> eval env trueBranch
    Vbool False -> eval env falseBranch
    _ -> error "Condition must evaluate to a boolean"
eval env (Lsend func args) =
  case eval env func of
    Vbuiltin f -> f (map (eval env) args)
    Vfob closure params body ->
      let argVals = map (eval env) args
          extendedEnv = zip params argVals ++ closure
      in eval extendedEnv body
    _ -> error "Application requires a function"
eval env (Llet v e1 e2) =
  let val = eval env e1
      extendedEnv = (v, val) : env
  in eval extendedEnv e2
eval env (Lfix bindings body) =
  let recEnv = [(v, eval recEnv e) | (v, e) <- bindings] ++ env
  in eval recEnv body
                  
---------------------------------------------------------------------------
-- Toplevel                                                              --
---------------------------------------------------------------------------

evalSexp :: Sexp -> Value
evalSexp = eval env0 . s2l

-- Lit un fichier contenant plusieurs Sexps, les évalues l'une après
-- l'autre, et renvoie la liste des valeurs obtenues.
run :: FilePath -> IO ()
run filename =
    do inputHandle <- openFile filename ReadMode 
       hSetEncoding inputHandle utf8
       s <- hGetContents inputHandle
       (hPutStr stdout . show)
           (let sexps s' = case parse pSexps filename s' of
                             Left _ -> [Ssym "#<parse-error>"]
                             Right es -> es
            in map evalSexp (sexps s))
       hClose inputHandle

sexpOf :: String -> Sexp
sexpOf = read

lexpOf :: String -> Lexp
lexpOf = s2l . sexpOf

valOf :: String -> Value
valOf = evalSexp . sexpOf
