{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, 
             OverlappingInstances, UndecidableInstances, 
             Rank2Types, GADTs, EmptyDataDecls,
             FlexibleContexts, FlexibleInstances,
             TypeSynonymInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Language.HJavaScript.Syntax
-- Copyright   :  (c) Joel Bjornson 2008
-- License     :  BSD-style
-- Maintainer  :  Joel Bjornson joel.bjornson@gmail.com
--                Niklas Broberg nibro@cs.chalmers.se
-- Stability   :  experimental
-----------------------------------------------------------------------------
module Language.HJavaScript.Syntax
  (
    -- * Primitive type classes.
    JType,
        
    -- * Fundamental data types.
    Exp(..),
    Rec,
    Var(..),
    Stmt(..),
    Block(..),
    
    -- * Data types and classes for object representation.
    IsClass,
    HasConstructor,
    IsDeref,
    
    -- * Misc
    AssignOp(..),
    BinOp(..),
    PlusOpType,    
    PostPre(..),
    Elses(..),
    IsNullable,
    IsFeature,
    JShow(..),
     
    -- * Types for functions and parameters.
    Args,
    ParamType,
    FormalParams(..),
    VarsToExps(..),
        
    -- * Array representation.
    Array(..),
       
    -- * Type synonyms.
    JInt, JString, JBool, JFloat, JVoid, JObject, JArray,
    
    -- * Type classes for expression representation.
    IsExp(..), 
    IsJString(..),
    IsJBool(..),
    IsJInt(..),
    IsJFloat(..),

    -- * Helper functions
    val, toBlock, deref, derefVar, propertyVar,
    call, methodCall, voidMethodCall, 
    methodCallNoArgs, voidMethodCallNoArgs,

    -- * Render function producing multi-line pretty-printed JavaScript code.
    renderBlock,
     
  ) where
   
import Text.PrettyPrint.HughesPJ
import Text.Printf(printf)
import Data.Char
import Data.Maybe

-- | JavaScript types
class JType t
instance JType String
instance JType Int
instance JType Bool
instance JType ()
instance JType Float
instance IsClass c => JType c
instance (Show t1, JType t1, Show t2, JType t2) => JType (t1 -> t2)

-------------------------------------------------------------------
-- JavaScript variable and expression representation
-------------------------------------------------------------------

-- Var represents JavaScript variables.
data Var t where
  JVar          :: String -> Var a
  JParam        :: String -> Var a
  JMember       :: String -> Var a
  JDerefVar     :: IsDeref d => d -> String -> Var a
  JArrayIndex   :: Exp (Array t) -> Exp Int -> Var t
  JPropertyVar  :: (IsDeref d, JShow p) => d -> Exp p -> Var a

instance Show (Var t) where
  showsPrec p var  = case var of
    JVar v           -> showString v
    JParam name      -> showString name
    JMember s        -> showString s
    JDerefVar o v    -> shows o . sDot . showString v
    JArrayIndex a ix ->  shows a . showString "[" . 
                               shows ix . showString "]"
    JPropertyVar c p -> shows c . showString "[" . shows p . showString "]"


-- We model records simply as tuples, for lack of a better record mechanism
data Rec a b

-- Exp represents JavaScript Expressions.
data Exp t where
  JInt         :: Int    -> Exp Int
  JFloat       :: Float  -> Exp Float
  JBool        :: Bool   -> Exp Bool
  JString      :: String -> Exp String
  JRec         :: Exp a -> Exp b -> Exp (Rec a b)
  JFst         :: Exp (Rec a b) -> Exp a
  JSnd         :: Exp (Rec a b) -> Exp b
  JConst       :: String -> Exp t
  JAssign      :: Var t -> Exp t -> Exp t
  JAssignWith  :: Var t -> AssignOp t -> Exp t -> Exp t
  JNeg         :: Exp t -> Exp t
  JNot         :: Exp Bool -> Exp Bool
  JBinOp       :: Exp t -> BinOp t r -> Exp t -> Exp r
  JIncrement   :: Num t => PostPre -> Var t -> Exp t
  JDecrement   :: Num t => PostPre -> Var t -> Exp t
  JIfOp        :: Exp Bool -> Exp t -> Exp t -> Exp t
  JCall        :: (Args e t) => Exp (t -> r) -> e -> Exp r
  JNew         :: (Args e t,  HasConstructor c e t) => c -> e -> Exp c
  JDelete      :: Var a -> Exp Bool
  JDeref       :: IsDeref d => d -> String -> Exp t
  JFunction    :: FormalParams a t => Maybe String -> a -> Block r -> Exp (t -> r)
  JThis        :: IsClass c => Exp c
  JBlock       :: Block () -> Exp ()
  JNull        :: IsNullable t => Exp t
  JCastObject  :: (IsClass c1, IsClass c2) => Exp c1 -> Exp c2
  JValueOf     :: Var t -> Exp t
  JIsImpl      :: (IsClass c , IsFeature f) => Exp c -> f -> Exp Bool
  JShow        :: JShow t => Exp t -> Exp String
  

-------------------------------------------------------------------
-- | Show for Exp
-------------------------------------------------------------------
instance Show (Exp t) where
  showsPrec p exp = case exp of
    JInt n                 ->  shows n
    JFloat f               ->  shows f
    JBool b                ->  if b then 
                                  showString "true" 
                                else 
                                  showString "false"
    JString s              ->  sJtr s
    JRec e1 e2             ->  showString "{fst:" . shows e1 . showString "," .
                               showString  "snd:" . shows e2 . showString "}"
    JFst e                 ->  shows e . showString ".fst"
    JSnd e                 ->  shows e . showString ".snd"
    JConst c               ->  showString c
    JAssign e1 e2          ->  shows e1 . sEq . shows e2
    JAssignWith e1 op e2   ->  shows e1 . shows op . shows e2
    JNeg e                 ->  showString "-" . (showParen True $ shows e)
    JNot e                 ->  showString "!" . (showParen True $ shows e)
    JBinOp e1 op e2        ->  shows e1 . shows op . shows e2
    JIncrement pp e        ->  if (pp == Pre) then 
                                  (showString "++" . shows e) 
                                else 
                                  (shows e . showString "++")
    JDecrement pp e        ->  if (pp == Pre) then 
                                  (showString "--" . shows e) 
                                else 
                                  (shows e . showString "--")
    JIfOp e1 e2 e3         ->  shows e1 . showString "?" . shows e2 . 
                               showString ":" . shows e3
    JCall x a2             ->  shows x . showsArgs a2
    JNew c a               ->  showString "new" . sSpace . shows c . showsArgs a
    JDelete v              ->  showString "delete" . sSpace . shows v
    JDeref o e             ->  shows o . sDot . showString e
    JFunction n fp b       ->  showString "function" .
                                sSpace .
                                sMaybe n . 
                                showsFParams fp . 
                                sBrack b
    JThis                  ->  showString "this"
    JBlock b               ->  shows b
    JNull                  ->  showString "null"
    JIsImpl ob c           ->  shows ob . sDot . showsFeature c
    JValueOf v             ->  shows v
    JCastObject e          ->  shows e
    JShow e                ->  shows e



-------------------------------------------------------------------
-- Type synonyms for Exp.
-------------------------------------------------------------------
type JInt       = Exp Int
type JString    = Exp String
type JBool      = Exp Bool
type JFloat     = Exp Float
type JVoid      = Exp ()
type JObject c  = Exp c
type JArray t   = Exp (Array t)


-------------------------------------------------------------------
-- Additional classes
-------------------------------------------------------------------

-- | Class for representing JavaScript "features", e.g. names of 
-- objects or functions. Example:  window `hasFeature` "alert"
class Show a => IsFeature a where
  showsFeature :: a -> ShowS

-- A class can be a feature
instance IsClass c => IsFeature c where
  showsFeature = shows
  

-- | Any string value can be a feature.
instance IsFeature String where
  showsFeature = showString


-- | Class that represents showable types
class JShow a where
 jshow :: a -> JString

instance JShow Int where
 jshow = jshow . JInt

instance JShow Float where
 jshow = jshow . JFloat
 
instance JShow Bool where
 jshow = jshow . JBool
 
instance JShow String where
 jshow = JString

instance JShow a => JShow (Exp a) where
 jshow = JShow

-- | Allows values to be compared to JNull. E.g. for checking that
-- an object is instantiated or is accessible.
class IsNullable a

-- | All JString values along with all objects and all functions can be null.
instance IsNullable String
instance IsClass c => IsNullable c
instance IsNullable (t -> r)

-------------------------------------------------------------------
-- JavaScript statements representation
-------------------------------------------------------------------
-- | Post or Pre prefix , i.e. --x or x++
data PostPre = Pst | Pre deriving Eq


-- | Assign Operator
data AssignOp t where
  PlusAssign  :: Num t => AssignOp t
  MinusAssign :: Num t => AssignOp t
  TimesAssign :: Num t => AssignOp t
  DivAssign   :: AssignOp Float
  ModAssign   :: AssignOp Int
  AndAssign   :: AssignOp Bool
  OrAssign    :: AssignOp Bool

instance Show (AssignOp t) where
  showsPrec p exp = case exp of
    PlusAssign  -> showString "+="
    MinusAssign -> showString "-="
    TimesAssign -> showString "*="
    DivAssign   -> showString "/="
    ModAssign   -> showString "%="
    AndAssign   -> showString "&="
    OrAssign    -> showString "|="
      

-- | Class for expression that may be "plussed".
-- Examples: 1 + 2, "ha" + "skell".
class PlusOpType a 
instance PlusOpType String
instance PlusOpType Int
instance PlusOpType Float

-- | Binary Operator
data BinOp t r where
  Plus        :: PlusOpType t => BinOp t t  
  Minus       :: Num t => BinOp t t
  Times       :: Num t => BinOp t t
  Div         :: Num t => BinOp t t
  Mod         :: BinOp Int Int
  And         :: BinOp Bool Bool
  Or          :: BinOp Bool Bool
  Equals      :: BinOp t Bool
  NotEquals   :: BinOp t Bool
  GThan       :: Num t => BinOp t Bool
  LThan       :: Num t => BinOp t Bool
  GEThan      :: Num t => BinOp t Bool
  LEThan      :: Num t => BinOp t Bool
  

instance Show (BinOp t r) where
  showsPrec p exp = case exp of
    Plus      -> showString " + "
    Minus     -> showString " - "
    Times     -> showString " * "
    Div       -> showString " / "
    Mod       -> showString " % "
    And       -> showString " && "
    Or        -> showString " || "
    Equals    -> showString " == "
    NotEquals -> showString " != "
    GThan     -> showString " > "
    LThan     -> showString " < "
    GEThan    -> showString " >= "
    LEThan    -> showString " <= "
    
    
data Stmt t where
  VarDecl       :: String -> Stmt ()
  VarDeclAssign :: String -> Exp t -> Stmt ()
  VarAssign     :: String -> Exp t -> Stmt ()
  ExpStmt       :: Exp t -> Stmt ()
  While         :: Exp Bool -> Block () -> Stmt ()
  DoWhile       :: Block () -> Exp Bool -> Stmt ()
  For           :: Stmt t1 -> Exp Bool -> Exp t2 -> Block () -> Stmt ()  
  ForIn         :: IsDeref d => Var String -> d -> Block () -> Stmt ()
  Break         :: Stmt ()
  Continue      :: Stmt ()
  Return        :: Exp t -> Stmt t
  If            :: Exp Bool -> Block t -> Elses t -> Stmt ()
  
data Elses t where
  Elseif  :: Exp Bool -> Block t -> Elses t -> Elses t
  Else    :: Block t -> Elses t
  NoElse  :: Elses ()  
  
instance Show (Stmt t) where
  showsPrec p stm = case stm of
    VarDecl s         ->  showString "var" . sSpace . showString s
    VarAssign s e     ->  showString s . showString " = " . shows e
    VarDeclAssign s e ->  showString "var" . sSpace . showString s .  
                          showString " = " . shows e 
    ExpStmt e         ->  shows e
    While e b         ->  showString "while" . showParen True (shows e) . 
                          sBrack b
    DoWhile b e       ->  showString "do" . sBrack b . showString "while" . 
                          showParen True (shows e)
    For e1 e2 e3 b    ->  showString "for" . 
                          showParen True (shows e1 . sc . shows e2 . 
                          sc . shows e3) . sBrack b
    ForIn v o b       ->  showString "for" .                          
                          showParen True (shows v . showString " in " . shows o) .
                          sBrack b
    Break             ->  showString "break"
    Continue          ->  showString "continue"
    Return e          ->  showString "return" . showString " " . shows e
    If e1 b1 b2       ->  showString "if" . showParen True (shows e1) . 
                          sBrack b1 . shows b2

instance Show (Elses t) where
  showsPrec p elses = case elses of
    Elseif e b els  -> showString "else" . sBrack b . shows els
    Else b          -> showString "else" . sBrack b
    NoElse          -> id 


sBrack s  = showString "{" . shows s . showString "}"
sSpace    = showString " "
sEq       = showString " = "
sDot      = showString "." 
sc        = showString ";"  
sNl       = showString "\n"
sJtr s    = showString "'" . showString (jEscape s) . showString "'"
sMaybe Nothing  = id
sMaybe (Just s) = showString s


-------------------------------------------------------------------
-- JavaScript block representation
-------------------------------------------------------------------
data Block t where
  EmptyBlock  :: Block ()
  Sequence    :: Block () -> Stmt t -> Block t

instance Show (Block t) where
  showsPrec p block = case block of
    EmptyBlock      -> id
    Sequence b stm  -> shows b . shows stm . sc
    
-------------------------------------------------------------------
-- Parameters and functions
-------------------------------------------------------------------

-- | Class for parameter types to JavaScript functions
class ParamType t

-- | Instanses for tuples,triples etc..
instance ParamType ()
instance JType t => ParamType t
instance (JType t1, JType t2 ) => ParamType (t1,t2)
instance (JType t1, JType t2, JType t3 ) => ParamType (t1,t2,t3)
instance (JType t1, JType t2, JType t3, JType t4) => ParamType (t1,t2,t3,t4)
instance (JType t1, JType t2, JType t3, JType t4, JType t5) => ParamType (t1,t2,t3,t4,t5)


-- | JFormal params represents parameters passed to a function along with their
-- corresponding types.
class (Show a , ParamType t) => FormalParams a t | a -> t where
  mkFParams :: forall b. (a -> b) -> Int -> a
  showsFParams :: a -> ShowS

instance FormalParams () () where
  mkFParams _ _ = ()
  showsFParams = shows  

instance ParamType t => FormalParams (Var t) t where
  mkFParams _ n = (JParam $ "param" ++ show n ++ "_0")  
  showsFParams = showParen True . shows


instance (ParamType (t1,t2)) => FormalParams (Var t1, Var t2) (t1,t2) where
  mkFParams _ n = (JParam $ "param" ++ show n ++ "_0", JParam $ "param" ++ show n ++ "_1")
  showsFParams = shows

instance (ParamType (t1,t2,t3)) => FormalParams (Var t1, Var t2, Var t3) (t1,t2,t3) where
  mkFParams _ n = (JParam $ "param" ++ show n ++ "_0", JParam $ "param" ++ show n ++ "_1", JParam $ "param" ++ show n ++ "_2")
  showsFParams = shows   


instance (ParamType (t1,t2,t3,t4)) => FormalParams (Var t1, Var t2, Var t3, Var t4) (t1,t2,t3,t4) where
  mkFParams _ n = (JParam $ "param" ++ show n ++ "_0", JParam $ "param" ++ show n ++ "_1", 
                    JParam $ "param" ++ show n ++ "_2", JParam $ "param" ++ show n ++ "_3")
  showsFParams = shows   

instance (ParamType (t1,t2,t3,t4,t5)) => FormalParams (Var t1, Var t2, Var t3, Var t4, Var t5) (t1,t2,t3,t4,t5) where
  mkFParams _ n = (JParam $ "param" ++ show n ++ "_0", JParam $ "param" ++ show n ++ "_1", 
                    JParam $ "param" ++ show n ++ "_2", JParam $ "param" ++ show n ++ "_3", 
                    JParam $ "param" ++ show n ++ "_4")
  showsFParams = shows   


-- | Args represents types that can be passed as arguments to
-- JavaScript functions.
class Show e => Args e t | e -> t where
  showsArgs :: e -> ShowS

instance Args () () where
  showsArgs = shows
  
instance Args (Exp t) t where
  showsArgs = showParen True . shows

instance  Args (Exp t1, Exp t2) (t1,t2) where
  showsArgs = shows
  
instance Args (Exp t1, Exp t2, Exp t3) (t1,t2,t3) where
  showsArgs = shows

instance Args (Exp t1, Exp t2, Exp t3, Exp t4) (t1,t2,t3,t4) where
  showsArgs = shows

instance Args (Exp t1, Exp t2, Exp t3, Exp t4, Exp t5) (t1,t2,t3,t4,t5) where
  showsArgs = shows


class VarsToExps v e | v -> e, e -> v where
  v2e :: v -> e

instance VarsToExps () () where
  v2e = id

instance VarsToExps (Var t) (Exp t) where
  v2e = val

instance VarsToExps (Var t1, Var t2) (Exp t1, Exp t2) where
  v2e (v1,v2) = (val v1,val v2)

instance VarsToExps (Var t1, Var t2, Var t3) (Exp t1, Exp t2, Exp t3) where
  v2e (v1,v2,v3) = (val v1,val v2,val v3)

instance VarsToExps (Var t1, Var t2, Var t3, Var t4) (Exp t1, Exp t2, Exp t3, Exp t4) where
  v2e (v1,v2,v3,v4) = (val v1,val v2,val v3,val v4)

instance VarsToExps (Var t1, Var t2, Var t3, Var t4, Var t5) (Exp t1, Exp t2, Exp t3, Exp t4, Exp t5) where
  v2e (v1,v2,v3,v4,v5) = (val v1,val v2,val v3,val v4,val v5)

--------------------------------------------------------------------
-- Object representation
-------------------------------------------------------------------
-- Class for representing 'class' elements in javascript (e.g. Math)
class Show c => IsClass c

-- | Class for binding objects with constructors. E.g. o = new Date();
class (IsClass c, Args e t) => HasConstructor c e t

-- | Class for derefable data types, used to allow the creation of 
-- dereferencing objects. Examples: Math.random() or document.write()
class Show r => IsDeref r

-- There are two kinds of dereferencing; either from classes or objects.
instance IsClass c  => IsDeref c
instance IsClass c  => IsDeref (Exp c)

-------------------------------------------------------------------
-- | Class for representing expressions.
-- First parameter is the expression, second a TBool for variable or constant.
-- Third parameter represents the type.
-------------------------------------------------------------------

class IsExp e t | e -> t where
  toExp :: e -> Exp t

instance IsExp (Exp t) t where
  toExp = id

instance IsExp String String where
  toExp = JString

instance IsExp Int Int where
  toExp = JInt

instance IsExp Float Float where
  toExp = JFloat

instance IsExp Bool Bool where
  toExp = JBool


-- | Class for JString expressions
class IsExp e String => IsJString e where
  toJString :: e -> Exp String
  toJString = toExp
  
-- | Class for JInt expressions
class IsExp e Int => IsJInt e where
  toJInt :: e -> Exp Int
  toJInt = toExp
  
-- | Class for JFloat expressions
class IsExp e Float => IsJFloat e where
  toJFloat :: e -> Exp Float
  toJFloat = toExp
  

-- | Class for JBool expressions
class IsExp e Bool => IsJBool e where
  toJBool :: e -> Exp Bool
  toJBool = toExp


instance IsExp e String => IsJString e

instance IsExp e Int => IsJInt e

instance IsExp e Float => IsJFloat e

instance IsExp e Bool => IsJBool e

-------------------------------------------------------------------
-- Array
-------------------------------------------------------------------
-- | Array representation
data Array t = Array deriving Show
instance IsClass (Array t)

-------------------------------------------------------------------
-- Helper functions
-------------------------------------------------------------------

-- Dereferencing
deref :: (IsDeref d) => String -> d -> Exp t
deref str obj = JDeref obj str

derefVar :: (IsDeref d) => String -> d -> Var a
derefVar str obj = JDerefVar obj str

propertyVar :: (IsDeref d, JShow p) => Exp p -> d -> Var a
propertyVar str obj = JPropertyVar obj str

-- Calling a function
call :: (Args e t) => Exp (t -> r) -> e -> Exp r
call = JCall

-- Method call returning an expression.
methodCall :: (Args e t1, IsDeref d) => String -> e -> d -> Exp t2
methodCall fun args obj = JCall (JDeref obj fun) args

-- Method call for void methods. Returns a Stmt since the return value is
-- not of any interest.
voidMethodCall :: (Args e t1, IsDeref a) => String -> e -> a -> Stmt ()
voidMethodCall fun args = ExpStmt . methodCall fun args

-- Method call for functions without input parameters. Just adds the extra
-- dummy argument ().
methodCallNoArgs ::IsDeref d => String -> d -> Exp t
methodCallNoArgs name = methodCall name ()

-- Method call for functions without input parameters returning void.
voidMethodCallNoArgs ::IsDeref d => String  -> d -> Stmt ()
voidMethodCallNoArgs name = ExpStmt . methodCall name ()


-- | Generates a Block from a Stmt.
toBlock :: Stmt t -> Block t
toBlock stm = Sequence EmptyBlock stm


-- | Get the value of a variable.
val :: Var t -> Exp t
val = JValueOf

-------------------------------------------------------------------
-- Pretty print JavaScript
-------------------------------------------------------------------

-- Pretty printing a block of JavaScript code using line breaks
-- and indentation. 
renderBlock :: Block r -> String
renderBlock = render . ppBlock

-- Number of spaces for indent
indent :: Int
indent = 2

ppBlock :: Block r -> Doc
ppBlock block = case block of
  EmptyBlock      -> empty
  Sequence b stm  -> ppBlock b $+$ ppStmt stm <> semi

ppVar :: Var a -> Doc
ppVar = text . show

ppExp :: Exp a -> Doc
ppExp exp = case exp of
  JInt n                 ->  int n  
  JFloat f               ->  float f
  JBool b                ->  if b then 
                                  text "true" 
                                else 
                                  text "false"
  JString s              ->  quotes $ text (jEscape s)
  JRec e1 e2             ->  text "{fst:" <> ppExp e1 <> comma <>
                               text "snd:" <>  ppExp e2 <> text "}"

  JFst e                 ->  ppExp exp <> text ".fst"
  JSnd e                 ->  ppExp e <> text ".snd"
  JConst c               ->  text c
  JAssign v e            ->  ppVar v <+> equals <+> ppExp e
  JAssignWith v op e     ->  ppVar v <+> text (show op) <+> ppExp e
  JNeg e                 ->  text "-" <> parens (ppExp e)
  JNot e                 ->  text "!" <> parens (ppExp e)
  JBinOp e1 op e2        ->  ppExp e1 <> text (show op) <> ppExp e2  
  JIncrement pp e        ->  if (pp == Pre) then 
                                  text "++" <> ppVar e 
                                else 
                                  ppVar e <> text "++"
  JDecrement pp e        ->  if (pp == Pre) then 
                                  text "--" <> ppVar e 
                                else 
                                  ppVar e <> text "--"
  JIfOp e1 e2 e3         ->  ppExp e1 <+> char '?' <+>  ppExp e2 <> 
                              char ':' <+> ppExp e3
  JCall f a              ->  ppExp f <> text (showsArgs a "")
  JNew c a               ->  text "new" <+>  text (show c) <> 
                              text (showsArgs a "")
  JDeref o e             ->  text (show o) <> char '.' <> text e
  JFunction n fp b       ->  text "function" <+> 
                                ppMaybe n <>
                                (text $ showsFParams fp "") <>
                                braces (ppBlock b)
  JThis                  ->  text "this"
  JBlock b               ->  ppBlock b
  JNull                  ->  text "null"
  JIsImpl ob c           ->  text (shows ob "") <> char '.' <> 
                              text (showsFeature c "")
  JValueOf v             ->  ppVar v
  JCastObject e          ->  ppExp e
  JShow e                ->  ppExp e

ppStmt :: Stmt a -> Doc 
ppStmt stm = case stm of
  VarDecl s         ->  text "var" <+>  text s
  VarAssign s e     ->  text s <+> text "=" <+> ppExp e
  VarDeclAssign s e ->  text "var" <+> text s <+> text "=" <+> ppExp e 
  ExpStmt e         ->  ppExp e
  While e b         ->  text "while" <> 
                        parens (ppExp e) <+> lbrace $+$ 
                        (nest indent $ ppBlock b) $+$ 
                        rbrace
  DoWhile b e       ->  text "do" <+> lbrace $+$ 
                        (nest indent  (ppBlock b)) $+$ 
                        rbrace <+> text "while" <+> parens (ppExp e)
  For e1 e2 e3 b    ->  text "for" <> 
                        parens (ppStmt e1 <> semi <+> ppExp e2 <> semi <+> ppExp e3) <+> 
                        lbrace $+$ 
                        (nest indent (ppBlock b)) $+$
                        rbrace
  Break             ->  text "break"
  Continue          ->  text "continue"
  Return e          ->  text "return" <+>  ppExp e
  If e1 b1 b2       ->  text "if" <+> parens (ppExp e1) <+> 
                          lbrace $+$  (nest indent (ppBlock b1)) $+$ 
                          rbrace $+$ 
                          ppElses b2

ppElses :: Elses a -> Doc
ppElses elses = case elses of
  Elseif e b els  -> text "else" <+> lbrace $+$  
                      (nest indent $ ppBlock b) $+$ rbrace <> ppElses els
  Else b          -> text "else" <+> lbrace $+$  
                      (nest indent $ ppBlock b) $+$ rbrace 
  NoElse          -> empty
  
ppMaybe (Just s) = text $ show s
ppMaybe Nothing = empty

-- | escape a Haskell String so that it is suitable for use as a
-- javascript string literal.
jEscape :: String -> String
jEscape [] = []
jEscape ('\b' : cs)  = "\\b"  ++ jEscape cs
jEscape ('\f' : cs)  = "\\f"  ++ jEscape cs
jEscape ('\n' : cs)  = "\\n"  ++ jEscape cs
jEscape ('\r' : cs)  = "\\r"  ++ jEscape cs
jEscape ('\t' : cs)  = "\\t"  ++ jEscape cs
jEscape ('\\'  : cs) = "\\\\" ++ jEscape cs
jEscape ('\'' : cs)  = "\\'"  ++ jEscape cs
jEscape (c : cs)    
  | ' ' <= c && c <= '~' = c : jEscape cs
  | otherwise = (printf "\\u%.4X" c) ++ jEscape cs
