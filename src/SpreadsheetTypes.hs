{-------------------------------------------------------------------------------
Module provides conversions from and to specific types,
used for online spreadsheets.

Spreadsheet cells or rows or columns could be of a certain predefined type.

Name           Database                 Haskell           Spreadsheet 
YesNo          Boolean (True = Yes)     Boolean           String 'Yes'/'No'
DayNight       Boolean (Day  = Yes)     Boolean           String 'Day'/'Night'

Integer        Int                      Int               String or Number
Decimal        Decimal                  Decimal           String or Number
Float          Double                   Double            String or Number

Text           Text                     String            String

Date           Timestamp                Timestamp         String

User           Int (FK)                 Int               String (username)

-------------------------------------------------------------------------------}

{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module SpreadsheetTypes ( 
    SCoord(..),
    SValue(..),
    SChange(..),
    SUpdate(..),
    changeToCell,
    updateToCells
) where

import Control.Concurrent

import           Data.Attoparsec
import           Data.Attoparsec.Number
import           Data.Data
import           Data.Char                       (isSpace, chr, ord)
import           Data.Maybe
import           Data.Time
import           Data.Time.Format                (readTime)
import           Data.Text                       (Text(..), pack, unpack)
import           Data.Text.Internal              (Text)
import           Data.Typeable
import           Control.Applicative
import           System.Random
import           Test.QuickCheck
import qualified Data.Aeson                  as  AE
import qualified Data.Aeson.Types            as  AT
import qualified Data.Aeson.TH               as  ATH
import qualified Data.ByteString             as  B
import qualified Data.ByteString.Lazy        as  BL
import qualified Data.ByteString.UTF8        as  BU
import qualified Data.Vector                 as  V
import qualified Control.Monad               as  M

--------------------------------------------------------------------------------
--Types-------------------------------------------------------------------------
-- Input from spreadsheet
data SCoord  = SCol Integer | SRow Integer              deriving (Eq, Show)
data SValue  = SYesNo Bool | SText Text | SDate UTCTime deriving (Eq, Show)
data SChange = SChange SCoord SCoord SValue SValue      deriving (Eq, Show)
type SUpdate = [SChange]

-- Output to spreadsheet
data SCell = SCell SCoord SCoord SValue                 deriving (Eq, Show)
type SCells = [SCell]

data Token    = Token String
data Request  = Request  Token SUpdate
data Response = Response Token SCells
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- FromJSON --------------------------------------------------------------------
instance AT.FromJSON SChange where
    parseJSON (AT.Array a) =
        case ((V.length a) == 4) of
            True  -> return $ sChange (a V.! 0) (a V.! 1) (a V.! 2) (a V.! 3)
            False -> fail "Wrong structure. Need a list of exactly four items."


sChange :: AT.Value -> AT.Value -> AT.Value -> AT.Value -> SChange
sChange (AT.Number (I row)) (AT.Number (I col)) v u 
    | (col == 0)  =  SChange (SCol col) (SRow row) (sText  v) (sText  u)
    -- | (col == 1)  =  SChange (SCol col) (SRow row) (sYesNo v) (sYesNo u)
    | otherwise   =  SChange (SCol col) (SRow row) (sText  v) (sText  u)
sChange col row _ _ = error "Invalid column or row"

instance AE.FromJSON Token where
    parseJSON (AE.Object v) = 
        Token <$> v AE..: "token"

instance AE.FromJSON Request where
    parseJSON (AE.Object v) = 
        Request <$> v AE..: "token" <*> v AE..: "cells"

-------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- FromJSON:simple conversions--------------------------------------------------
sYesNo :: AT.Value -> SValue
sYesNo (AT.String val) | (val == "Yes")  =  SYesNo True
                       | (val == "No")   =  SYesNo False
                       | otherwise       =  error "sYesNo: Invalid input value"
sYesNo _   = error "sYesNo: Invalid value type"


sText :: AT.Value -> SValue
sText (AT.String val) = SText val
sText (AT.Number val) = SText (pack $ show val)
sText _               = error "sText: Invalid value type"

{-
sDate :: AT.Value -> SValue
sDate (AT.String val) = 
    case (SDate (readTime defaultTimeLocale "%d %b %Y" val :: UTCTime)) of 
        Nothing          -> error "Invalid date format. Expected %d %b %Y"
        Just (UTCTime t) -> t
-}
-------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- ToJSON ----------------------------------------------------------------------
instance AT.ToJSON SChange where
    toJSON (SChange c r v u) = 
        AT.Array (V.fromList [AT.toJSON r,AT.toJSON c,AT.toJSON v,AT.toJSON u])
    
instance AT.ToJSON SCell where
    toJSON (SCell c r v) = 
        AT.Array (V.fromList [AT.toJSON r,AT.toJSON c,AT.toJSON v])

instance AT.ToJSON SValue where
    toJSON (SText  t) = AT.String t
    toJSON (SYesNo t) | (t == True ) = AT.String "Yes"
                      | (t == False) = AT.String "No"

instance AT.ToJSON SCoord where
    toJSON (SCol t) = AT.Number (fromInteger t)
    toJSON (SRow t) = AT.Number (fromInteger t)
    
instance AT.ToJSON Response where
 toJSON (Response (Token token) cells) =
    AT.object [ "token" AT..= token
              , "cells" AT..= AT.toJSON cells]
           
--------------------------------------------------------------------------------


changeToCell :: SChange -> SCell
changeToCell (SChange c r v u) = (SCell c r u)

updateToCells :: SUpdate -> SCells
updateToCells = map changeToCell


--------------------------------------------------------------------------------
-- Testing ---------------------------------------------------------------------
instance Arbitrary SChange where
    arbitrary = do
        NonNegative row <- arbitrary
        --NonNegative col <- arbitrary
        let col = 0
        x <- arbitrary
        y <- arbitrary
        let 
        let xt = pack $ Prelude.take 5 $ repeat (x::Char)
        let yt = pack $ Prelude.take 5 $ repeat (y::Char)
        return $ SChange (SCol col) (SRow row) (SText xt) (SText yt)


testSUpdate :: SUpdate -> Bool
testSUpdate update = ((AE.decode . AE.encode) update) == (Just update)
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
--Main--------------------------------------------------------------------------
main = do
    let k = SChange (SCol 0) (SRow 2) (SText "a") (SText "b")
    
    let z = (((AE.decode . AE.encode) (Just k)) == (Just k))
    print z
    
    case (True) of
        True -> do 
            putStrLn "aa" 
            return () 
        False -> return ()
    --(quickCheck . verbose) testSUpdate
    (quickCheck) testSUpdate
