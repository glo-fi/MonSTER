{-# LANGUAGE OverloadedStrings, AllowAmbiguousTypes, TypeSynonymInstances #-}
import System.Environment
import System.IO hiding (appendFile)
import Data.Set hiding (map)
import Data.List hiding (union, insert)
import Data.Text (Text, pack, lines) 
import Data.Text.IO
import Data.Either
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer as L
import Control.Monad.Combinators.Expr
import Data.Void
import Control.Monad.State.Lazy hiding(lift)
import Control.Monad.Trans.Except

-- expressions and programs
type Id = String                                 -- identifiers
data Exp = Atm Id                                -- atomic procedure
         | Cal Id                                -- call a defined procedure 
         | Cnd Exp Exp                           -- non-deterministic branch 
         | Seq Exp Exp deriving Eq               -- sequence
instance Show Exp where
  show (Atm a) = a 
  show (Cal f) = f 
  show (Cnd e1 e2) = "(" ++ show e1 ++ " ? " ++ show e2 ++ ")"
  show (Seq e1 e2) = "(" ++ show e1 ++ " ; " ++ show e2 ++ ")"
type Prg = [(Id, Exp)]                           -- program 


type Parser = Parsec Void Text

sc :: Parser ()
sc = L.space
  space1
  (L.skipLineComment "//")
  (L.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol           = L.symbol sc
parens           = between (Main.symbol "(") (Main.symbol ")")
specialChars = ['-', '_', '$', '<', '>','(', ')', '/',
                '[' ]

inClass :: String -> Char -> Bool
inClass "" = const False
inClass (a:'-':b:xs) = \c -> (c >= a && c <= b) || f c where f = inClass xs
inClass (x:xs) = \c -> c == x || f c where f = inClass xs

expr = makeExprParser term table <?> "expression"


pAtm :: Parser Exp
pAtm = Atm <$> Main.lexeme ((:) <$> satisfy (== '&') <*> many (try alphaNumChar <|> satisfy (inClass specialChars)) <?> "variable")

pCal :: Parser Exp
pCal = Cal <$> Main.lexeme ((:) <$>  (letterChar <|> satisfy (inClass specialChars)) <*> many (try alphaNumChar <|> satisfy (inClass specialChars)) <?> "variable")

pId :: Parser Id
pId = Main.lexeme ((:) <$> (letterChar <|> satisfy (inClass specialChars))  <*> many (try alphaNumChar <|> try (satisfy (inClass specialChars))) <?> "variable")



term :: Parser Exp
term = choice
  [ parens expr
  ,  pAtm
  ,  pCal
  ]



table = [ --[ prefix "&" Atm],
          [ Main.binary "?" Cnd],
          [ Main.binary ";" Seq]]



binary name f = InfixR (f <$ Main.symbol name)
prefix name f = Prefix (f <$ Main.symbol name)
postfix name f = Postfix (f <$ Main.symbol name)

prgParse :: Parser (Id, Exp)
prgParse = do
  sc
  id <- pId <?> "valid id"
  sc
  void (char ':')
  void (char '=')
  sc
  exp <- expr <?> "valid exp"
  return (id, exp)

sepParse :: Parser Prg
sepParse = do
  list <- try prgParse `sepBy` (Main.symbol "-")
  return $ list

pparse = parseTest prgParse

parseFromFile p file = runParser p file <$> Data.Text.IO.readFile file

showSingle :: (Id, Exp) -> String
showSingle (ids, exp) = id ids ++ " := " ++ show exp


parsedEqn :: Either (ParseErrorBundle Text Void) Prg -> IO()
parsedEqn (Right prg) = System.IO.writeFile "eqn.output" (show $ (eqn prg :: Eqn AMon) )
parsedEqn (Left e) = System.IO.putStrLn "error"

parsedLfp :: Either (ParseErrorBundle Text Void) Prg -> IO()
parsedLfp (Right prg) = System.IO.writeFile "lfp.output" (show $ lfp ( (eqn prg :: Eqn AMon) ))
parsedLfp (Left e) = System.IO.putStrLn "error"

parsedChk :: (Eq a, Ord a, Mon a) => Either (ParseErrorBundle Text Void) Prg -> Exp -> Prp a -> IO()
parsedChk (Right prg) exp prp = System.IO.putStrLn (show $ (chk prg exp prp))
parsedChk (Left  e) exp prp = System.IO.putStrLn "error"

---------------------------------------------------------------------------------------------------------

type Sgm = Set Id                                -- appealling atomic procedures 
type Flow = [Id]                                 -- call flow


pa = [("f", Cnd (Atm "b") (Seq (Atm "a") (Cal "f")))]
sa = fromList ["a", "b"] 
ea = Cal "f" 

-- type expressions
data TExp a = Emp                                -- empty type 
            | Cst a                              -- constant
            | Var Id                             -- variable 
            | Lfp Id (TExp a)                    -- least fixed point 
            | Add (TExp a) (TExp a)              -- union 
            | Con (TExp a) (TExp a) deriving Eq  -- concatenation
instance Show a => Show (TExp a) where
  show Emp         = show "Z" 
  show (Cst c)     = show c
  show (Var v)     = v 
  show (Lfp v e)   = "(lfp " ++ v ++ " . " ++ show e ++ ")" 
  show (Add e1 e2) = "(" ++ show e1 ++ " + " ++ show e2 ++ ")"
  show (Con e1 e2) = "(" ++ show e1 ++ " . " ++ show e2 ++ ")"

-- inference type expressions
inf :: Prg -> Sgm -> Exp -> TExp Id 
inf prg sgm = rec Data.Set.empty
  where
    rec env (Atm a)     = if member a sgm then Cst a else Cst "" 
    rec env (Cal f)     = case lookup f prg of
                            Nothing -> error "Undefined procedure!"
                            Just e  -> if member f env
                                       then Var f
                                       else Lfp f (rec (insert f env) e)
    rec env (Cnd e1 e2) = Add (rec env e1) (rec env e2)
    rec env (Seq e1 e2) = Con (rec env e1) (rec env e2)


pb = [("f", Cnd (Seq (Atm "a") (Seq (Cal "f") (Atm "b"))) (Cal "g")),
      ("g", Cnd (Seq (Atm "c") (Cal "f")) (Atm "d"))]
sb = fromList ["a", "b"]
eb = Cal "f"

-- monoids 
class (Eq a, Ord a, Show a) => Mon a where
  unit :: a
  mult :: a -> a -> a
  lift :: Id -> a

data AMon = U | SOURCE | SINK | SOURCE_SINK | SINK_SOURCE deriving (Eq, Ord, Show)
instance Mon AMon where
  unit = U
  mult a  U  = a
  mult U  a  = a
  mult SOURCE  SOURCE  = SOURCE 
  mult SINK  SINK  = SINK
  mult SINK_SOURCE SOURCE  = SINK_SOURCE 
  mult SINK  _  = SINK_SOURCE
  mult _  _  = SOURCE_SINK
  lift "&SOURCE" = SOURCE
  lift "&SINK" = SINK 
  lift _   = U 

type ZMon = Int
instance Mon ZMon where
  unit = 0 
  mult = (+) 
  lift "a" = 1
  lift "b" = -1
  lift _   = 0

-- auxiliary functions 
tmap :: (a -> b) -> TExp a -> TExp b 
tmap f Emp         = Emp 
tmap f (Var v)     = Var v 
tmap f (Cst a)     = Cst (f a) 
tmap f (Lfp v t)   = Lfp v (tmap f t) 
tmap f (Add t1 t2) = Add (tmap f t1) (tmap f t2) 
tmap f (Con t1 t2) = Con (tmap f t1) (tmap f t2) 

setAdd :: Ord a => TExp a -> Set a
setAdd (Add t1 t2)        = union (setAdd t1) (setAdd t2)
setAdd (Cst a)            = singleton a 
setAdd _                  = Data.Set.empty

isCst :: TExp a -> Bool
isCst (Cst _)             = True
isCst _                   = False

isAdd :: TExp a -> Bool
isAdd (Add _ _)           = True
isAdd _                   = False 

isConCst :: TExp a -> Bool
isConCst (Con (Cst _) _)  = True
isConCst _                = False

isContCst :: TExp a -> Bool
isContCst (Con _ (Cst _)) = True
isContCst _               = False

-- simplify type expressions
simpl :: (Eq a, Ord a, Show a, Mon a) => TExp a -> TExp a
simpl (Add Emp t)           = simpl t 
simpl (Add t Emp)           = simpl t
simpl (Add (Cst a) t)       = let ta = simpl t
                              in if member a (setAdd ta) then ta
                                 else if ta == Emp then (Cst a)
                                      else (Add (Cst a) ta)
simpl (Add t (Cst a))       = simpl (Add (Cst a) t) 
simpl (Add t1 t2)           = let ta = simpl t1  
                                  tb = simpl t2
                              in if ta == tb then ta
                                 else if ta == Emp || tb == Emp ||
                                         isCst ta  || isCst tb
                                      then simpl (Add ta tb) 
                                      else Add ta tb
simpl (Con Emp _)           = Emp
simpl (Con _ Emp)           = Emp
simpl (Con (Cst a) (Cst b)) = Cst (mult a b)
simpl (Con t (Add t1 t2))   = simpl (Add (Con t t1) (Con t t2))
simpl (Con (Add t1 t2) t)   = simpl (Add (Con t1 t) (Con t2 t))
simpl (Con (Cst a) 
           (Con (Cst b) t)) = simpl (Con (Cst (mult a b)) t) 
simpl (Con (Con t (Cst a)) 
           (Cst b))         = simpl (Con t (Cst (mult a b))) 
simpl (Con t1 t2)           = let ta = simpl t1
                                  tb = simpl t2
                              in if ta == Cst unit then tb 
                                 else if tb == Cst unit then ta
                                 else if ta == Emp || tb == Emp || 
                                         (isCst ta && isCst tb) ||
                                         isAdd ta || isAdd tb ||
                                         (isCst ta && isConCst tb) ||
                                         (isContCst ta && isCst tb)
                                      then simpl (Con ta tb) 
                                      else Con ta tb 
simpl (Lfp v t)             = Lfp v (simpl t)
simpl t                     = t 

ta = Con (Cst "") (Add (Add (Cst "a") (Con (Cst "b") (Add (Cst "b") (Cst "")))) Emp)
tb = Lfp "f" (Con (Cst "b") (Add Emp (Con (Cst "a") (Var "f"))))
tc = Con (Con (Add (Cst "a") (Cst "b")) (Cst "b")) (Cst "a")

-- least fixed points
type Eqn a = [(Id, TExp a)]                      -- equation

eqn :: (Eq a, Ord a, Mon a) => Prg -> Eqn a 
eqn prg = map (\(f,e) -> (f, simpl $ rec e)) prg
  where
    rec (Atm a)     = Cst (lift a)
    rec (Cal f)     = Var f
    rec (Cnd e1 e2) = Add (rec e1) (rec e2) 
    rec (Seq e1 e2) = Con (rec e1) (rec e2)

lfp :: (Eq a, Ord a, Mon a) => Eqn a -> Eqn a 
lfp eqn = fix f init
  where
    fix f vs = if set vs == set (f vs) then vs else fix f (f vs) 
    f vs = map (\(d,t) -> (d, simpl $ ass vs t)) eqn
    init = map (\(d,_) -> (d, Emp)) eqn 
    ass vs (Var v)     = case lookup v vs of
                           Nothing -> error ("Unassgined variable:" ++ v ++ "!")
                           Just t  -> t
    ass vs (Add t1 t2) = Add (ass vs t1) (ass vs t2) 
    ass vs (Con t1 t2) = Con (ass vs t1) (ass vs t2)
    ass vs t           = t
    set vs = map (\(d, v) -> (d, setAdd v)) vs 

-- checking properties
type Prp a = Set a                               -- property 

chk :: (Eq a, Ord a, Mon a) => Prg -> Exp -> Prp a -> Bool
chk prg exp prp = let vs = lfp (eqn prg)
                      s  = setAdd (simpl (rep vs exp)) 
                  in isSubsetOf s prp  
  where
    rep vs (Atm a)     = Cst (lift a)
    rep vs (Cal f)     = case lookup f vs of
                           Nothing -> error ("Undefined procedure:" ++ f ++ "!") 
                           Just t  -> t 
    rep vs (Cnd e1 e2) = Add (rep vs e1) (rep vs e2) 
    rep vs (Seq e1 e2) = Con (rep vs e1) (rep vs e2)

main = do
  let aus = parseFromFile sepParse "edited_coalition_apk.output"
  aus >>= parsedLfp
  return ()





    
