{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE PartialTypeSignatures      #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}


-- | Receive 
module Plutus.Contracts.OnChain.TokenCuratedTrustRegistry
    ( 
      TokenMoveDatum (..)
    -- * Scripts
    -- * Address
    ) where

import qualified Data.ByteString.Char8 as C
import           Plutus.V1.Ledger.Api  (Address, PubKeyHash, ScriptContext, Validator)
import qualified Ledger
import qualified Ledger.Ada            as Ada
import qualified Ledger.Constraints    as Constraints
import           Ledger.Tx             (ChainIndexTxOut (..))
import           Playground.Contract
import           Plutus.Contract
import           PlutusTx.Builtins.Class (stringToBuiltinByteString)
import qualified PlutusTx
import           PlutusTx.Prelude      hiding (pure, (<$>))
import qualified Prelude               as Haskell
import qualified Plutus.Script.Utils.V1.Typed.Scripts as Scripts

data RegistryParams = RegistryParams {
      tokenName :: TokenName,
      controlTokenName :: TokenName
}


data TokenMoveDatum = TokenMoveDatum {
      voteReference :: PubKeyHash
   } deriving anyclass (PlutusTx.ToData, PlutusTx.FromData, PlutusTx.UnsafeFromData)


PlutusTx.makeLift ''TokenMoveDatum

-- We have two types of token: 
-- control token and issue token
--
-- Agents:
--    Issuers (have did and address)
--    Voters 
--         (decide who can be an issuer, holders of owning tokens)
--    Verifier (proxy for voters)
--
-- Control Token
-- Vote Token
--  
--
-- Issuer    
--    Can mint issuer tokens, if can complete a vote transaction, where each utxo is a control vote token
--    
-- Issuers are represented by address, for conversion between deed and address DidAddress tokens are used.

data VotingPurpose = VotingPurposeMint
                    |
                     VotingPurposeBurn
                    |
                     ControlVotingPurporeMint
                    |
                     ControlVotingPurporeBurn

data VotingRoundParams = VotingRoundParams {
        name :: BinaryString,
        redeemCode :: BinaryString,
        votingPower :: Value
        finishTime :: PosixTime
}


data VoteTokenParams = VoteTokenParams {
        purpose :: VotingPurpose,
        controlToken :: AssetClass,
        roundParams :: VoteRoundParams
}


mintVoteToken :: VotingTokenParams -> BuiltinData -> BuiltinData -> ()
mintVoteToken params redeemerData ctxData =
       if (transactionControlVotingAmount >= paramControlVotingAmount)
          -- TODO: add check for params in datum
          ()
       else
          error()
         where 
            transactionControlVotingAmount = case Map.lookup controlToken txFee of
                                                Nothing -> 0
                                                Just tokens -> case Map.lookup controlTokenName tokens of
                                                   Nothing -> 0
                                                   Jast amount -> amount
            paramControlVotingAmount = case Map.lookup controlToken
            txData = Ledger.txInfoData txInfo
            txFee = (getValue (Ledger.txInfoFee txInfo))
            txInfo = Ledger.scriptContextTxInfo ctx
            ctx = PlutusTx.unsafeFromBuiltinData ctxData



validateClaim :: TokenMoveDatum -> _ -> ScriptContext -> Bool
validateClaim tokenMoveDatum _ _ =  False -- TODO: implement


