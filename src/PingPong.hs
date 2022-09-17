{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module PingPong where

import           Control.Monad        hiding (fmap)
import           Data.Aeson           (ToJSON, FromJSON)       
import           Data.Map             as Map                  
import           Data.Text            (Text)
import           Data.Void            (Void)
import           GHC.Generics         (Generic)
import           Plutus.Contract      as Contract
import           PlutusTx             (Data (..))
import qualified PlutusTx
import           PlutusTx.Eq          (Eq (..))
import           PlutusTx.Prelude     hiding (Semigroup(..), unless)
import           Ledger               hiding (singleton)
import           Ledger.Constraints   (TxConstraints)
import qualified Ledger.Constraints   as Constraints
import qualified Ledger.Typed.Scripts as Scripts
import           Ledger.Ada           as Ada
import           Ledger.Value
import           Playground.Contract  (printJson, printSchemas, ensureKnownCurrencies, stage, ToSchema, Show)
import           Playground.TH        (mkKnownCurrencies, mkSchemaDefinitions)
import           Playground.Types     (KnownCurrency (..))
import           Prelude              (IO, Semigroup (..), String)
import           Text.Printf          (printf)

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- ON-CHAIN CODE  

data Player = Player1 | Player2
    deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

instance Eq Player where
    {-# INLINABLE (==) #-}
    Player1 == Player1 = True
    Player2 == Player2 = True
    _       == _       = False

PlutusTx.unstableMakeIsData ''Player

data GameDatum = GD
    { gdPlayer1      :: !PaymentPubKeyHash
    , gdPlayer2      :: !PaymentPubKeyHash
    , gdDeadline     :: !POSIXTime
    , gdBallLocation :: !Player
    } deriving Show

PlutusTx.unstableMakeIsData ''GameDatum

{-# INLINABLE gameToken #-}
gameToken :: AssetClass
gameToken = AssetClass (CurrencySymbol "50696e67506f6e67", TokenName "PingPong")
-- Fix Me


{-# INLINABLE mkGameValidator #-}
mkGameValidator :: GameDatum -> () -> ScriptContext -> Bool
mkGameValidator dat _ ctx =
    traceIfFalse "Game NFT not found" $ assetClassValueOf (txOutValue ownInput) gameToken == 1

    where
        ownInput :: TxOut
        ownInput = case findOwnInput ctx of
            Nothing -> traceError "input missing"
            Just i  -> txInInfoResolved i

data Typed
instance Scripts.ValidatorTypes Typed where
    type instance DatumType Typed = GameDatum
    type instance RedeemerType Typed = ()

typedGameValidator :: Scripts.TypedValidator Typed
typedGameValidator = Scripts.mkTypedValidator @Typed
    $$(PlutusTx.compile [|| mkGameValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
        where
            wrap = Scripts.wrapValidator @GameDatum @()


gameValidator :: Validator
gameValidator = Scripts.validatorScript typedGameValidator

gameAddress :: Ledger.Address
gameAddress = scriptAddress gameValidator





-- OFF-CHAIN CODE

data StartParams = StartParams
    { spDuration       :: !Integer
    , spOpponentPkh    :: !PaymentPubKeyHash
    } deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

startGame :: forall w s. StartParams -> Contract w s Text ()
startGame sp = do
    pkh <- Contract.ownPaymentPubKeyHash
    let v       = (<>) (lovelaceValueOf 1) (assetClassValue gameToken 1)
        gDatum  = GD pkh (spOpponentPkh sp) (POSIXTime 0) Player2                                          -- TODO calculate the DEADLINE   Use "now"
        tx   = Constraints.mustPayToTheScript gDatum v
    ledgerTx <- submitTxConstraints typedGameValidator tx
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    logInfo @Prelude.String $ printf "PING! Game started." 

hitBall :: forall w s. Contract w s Text ()
hitBall = do
    logInfo @Prelude.String $ printf "Not yet implemented!"
-- TODO: implement function


type GameSchema = Endpoint "start" StartParams .\/ Endpoint "hit" ()

endpoints :: Contract () GameSchema Text ()
endpoints = awaitPromise (start `select` hit) >> endpoints
  where
    start  = endpoint @"start" startGame
    hit = endpoint @"hit" $ const hitBall


mkSchemaDefinitions ''GameSchema

mkKnownCurrencies []  