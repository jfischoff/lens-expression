> {-# LANGUAGE GADTs #-}
> {-# LANGUAGE TypeFamilies #-}
> {-# LANGUAGE PolyKinds #-}
> {-# LANGUAGE DataKinds #-}
> {-# LANGUAGE ScopedTypeVariables #-}
> {-# LANGUAGE RecordWildCards #-}
> {-# LANGUAGE LambdaCase #-}
> {-# LANGUAGE TupleSections #-}
> {-# LANGUAGE TemplateHaskell #-}
> {-# LANGUAGE RankNTypes #-} 
> module Expr where
> import Control.Lens
> import Data.Singletons
> import Data.Singletons.TH
> import Data.Aeson.Lens
> import qualified Data.List.NonEmpty as NEL
> import Data.List.NonEmpty (NonEmpty (..))
> import qualified Data.DList         as DList
> import Data.DList (DList)
> import Text.Trifecta
> import Control.Applicative
> import Data.Maybe
> import Data.Monoid
> import Data.Aeson
> import Data.Scientific
> import qualified Data.ByteString.Lazy as BSL

The `lens` 'optics' are sometimes what I want for command line utilities.

Building a expression of optics is a little tricky AFAICT

For the sake of this demo, lets say I want to make a command line utility for manipulating JSON. `lens' comes with most of what I need.

To my lenses compose I need codes of all the types I am working with.

> singletons [d| 
>   data Code 
>       = ValueT
>       | ArrayT
>       | ObjectT
>       | ScientificT
>       | DoubleT
>       | IntegerT
>       | BoolT
>       | UnitT
>       | TextT
>       | StringT
>       | ByteStringT
>       | PrimitiveT |]

> type family Decode (a :: Code) :: *
> type instance Decode ValueT      = Value
> type instance Decode ScientificT = Scientific
> type instance Decode StringT     = String
> type instance Decode ByteStringT = BSL.ByteString
> type instance Decode PrimitiveT  = Primitive
> type instance Decode DoubleT     = Double
> type instance Decode IntegerT    = Integer
> type instance Decode BoolT       = Bool
> type instance Decode UnitT       = ()
> type instance Decode ObjectT     = Object
> type instance Decode ArrayT      = Array

Now I need a way to wrap up my lenses to temporarily get rid of the type variables

> data AnyOptic where 
>   AnyLens :: (SingI a, SingI b)
>           => (Sing a) 
>           -> (Sing b) 
>           -> Lens' (Decode a) (Decode b)
>           -> AnyOptic
>   AnyPrism :: (SingI a, SingI b)
>           => (Sing a) 
>           -> (Sing b) 
>           -> Prism' (Decode a) (Decode b)
>           -> AnyOptic

I also will need a list of AnyLenses for dealing type class instances

> type AnyLensMethod = NonEmpty AnyOptic

I can compose the 'AnyLens' if the types match up

> anyLensCompose :: AnyOptic -> AnyOptic -> Maybe AnyOptic
> anyLensCompose (AnyLens a b l) (AnyLens b' c l') = case (b, b') of
>   (SValueT, SValueT) -> return $ AnyLens a c (l . l')
>   otherwise_       -> Nothing

That is the tricky part. 

Collect all of the ones that match

> anyLensMethodCompose :: AnyLensMethod -> AnyLensMethod -> Maybe AnyLensMethod
> anyLensMethodCompose x y 
>   = NEL.nonEmpty $ mapMaybe (uncurry anyLensCompose) $ zip (NEL.toList x) (NEL.toList y)

Now I need a way to parse the lenses

> pCompose :: Parser (DList AnyLensMethod -> DList AnyLensMethod -> DList AnyLensMethod)
> pCompose = (<>) <$ symbol " "

> pLenses :: Parser (DList AnyLensMethod)
> pLenses = pTerm `chainl1` pCompose

> pTerm 
>   =  DList.singleton <$> pTerm' 
>  <|> pLenses

> pTerm'  
>    =  pNumber
>   <|> pDouble
>   <|> pInteger
>   <|> pIntegral
>   <|> pPrimitive
>   <|> pString
>   <|> pBool
>   <|> pNull
>   <|> pValue
>   <|> pObject
>   <|> pArray
>   <|> pNonNull
>   <|> pKey
>   <|> pMembers
>   <|> pNth
>   <|> pValues
>   <|> pJSON

Okay I need to rethink my type
it is actual one of several possible lenses

> pNumber :: Parser AnyLensMethod
> pNumber = (AnyPrism SStringT SScientificT _Number :|
>   [ AnyPrism SByteStringT SScientificT _Number
>   , AnyPrism SScientificT SScientificT _Number
>   , AnyPrism SValueT      SScientificT _Number
>   , AnyPrism SPrimitiveT  SScientificT _Number
>   ]) <$ symbol "number"

> pDouble :: Parser AnyLensMethod
> pDouble = ( AnyPrism SStringT     SDoubleT _Double :|
>           [ AnyPrism SByteStringT SDoubleT _Double
>           , AnyPrism SScientificT SDoubleT _Double
>           , AnyPrism SValueT      SDoubleT _Double
>           , AnyPrism SPrimitiveT  SDoubleT _Double
>           ]) <$ symbol "double"

> pInteger :: Parser AnyLensMethod
> pInteger = ( AnyPrism SStringT     SIntegerT _Integer :|
>            [ AnyPrism SByteStringT SIntegerT _Integer
>            , AnyPrism SScientificT SIntegerT _Integer
>            , AnyPrism SValueT      SIntegerT _Integer
>            , AnyPrism SPrimitiveT  SIntegerT _Integer
>            ]) <$ symbol "integer"

This should just be floor

-- pIntegral :: Parser AnyLensMethod
-- pIntegral = undefined

> pPrimitive :: Parser AnyLensMethod
> pPrimitive = undefined

> pString :: Parser AnyLensMethod
> pString = undefined

> pBool :: Parser AnyLensMethod
> pBool = undefined

> pNull :: Parser AnyLensMethod
> pNull = undefined

> pValue :: Parser AnyLensMethod
> pValue = undefined

> pObject :: Parser AnyLensMethod
> pObject = undefined

> pArray :: Parser AnyLensMethod
> pArray = undefined

> pNonNull :: Parser AnyLensMethod
> pNonNull = undefined

> pKey :: Parser AnyLensMethod
> pKey = undefined

> pMembers :: Parser AnyLensMethod
> pMembers = undefined

> pNth :: Parser AnyLensMethod
> pNth = undefined

> pValues :: Parser AnyLensMethod
> pValues = undefined

> pJSON :: Parser AnyLensMethod
> pJSON = undefined





