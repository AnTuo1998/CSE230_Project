{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Buff
  ( BuffType (..),
    BuffState,
    buffCoord,
    initBuff,
    buffT,
    newBuffFac,
  )
where

-- import BB(Game(..))
import Control.Applicative ((<|>))
import Control.Lens hiding ((:<), (:>), (<|), (|>))
import Control.Monad (guard)
import Control.Monad.Extra (orM)
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State
import Data.List
import Data.Maybe (fromMaybe, isNothing)
import Data.Sequence (Seq (..), (<|))
import qualified Data.Sequence as S
import Data.Tuple.Select (Sel1 (sel1), Sel2 (sel2), Sel3 (sel3))
import Linear.V2 (V2 (..), _x, _y)
import System.IO

data BuffType = FireBall | Split deriving (Eq, Show)

data BuffState = BuffState
  { _buffT :: BuffType,
    _buffCoord :: V2 Int
  }
  deriving (Eq, Show)

makeLenses ''BuffState

-- initBuff = S.fromList [BuffState{_buffCoord=V2 10 3, _buffT=FireBall}]
initBuff = S.empty

newBuffFac coord t = S.fromList [BuffState {_buffCoord = coord, _buffT = t}]
