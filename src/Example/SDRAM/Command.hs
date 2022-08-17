{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module Example.SDRAM.Command where

import Clash.Prelude

import Example.SDRAM.Types

data SDRAMCmd (d :: Domain) (s :: SDRAMSpec) = SDRAMCmd
   { sDRAMA  :: BitVector (SDRAMAddrWidth d s)
   , sDRAMCKE :: Bit
   , sDRAMCSN :: Bit
   , sDRAMLDQM :: Bit
   , sDRAMUDQM :: Bit
   , sDRAMWeN :: Bit
   , sDRAMRasN :: Bit
   , sDRAMCasN :: Bit
   , sDRAMBa :: BitVector (SDRAMBankWidth d s)
   }

deriving instance KnownSDRAMSpec d s => Eq (SDRAMCmd d s)
deriving instance KnownSDRAMSpec d s => Show (SDRAMCmd d s)
deriving instance KnownSDRAMSpec d s => Generic (SDRAMCmd d s)
deriving instance KnownSDRAMSpec d s => NFDataX (SDRAMCmd d s)
deriving instance KnownSDRAMSpec d s => BitPack (SDRAMCmd d s)

instance KnownSDRAMSpec d s => Default (SDRAMCmd d s) where
  def = SDRAMCmd
          { sDRAMA = 0
          , sDRAMCKE = 1
          , sDRAMCSN = 0
          , sDRAMLDQM = 1
          , sDRAMUDQM = 1
          , sDRAMWeN = 1
          , sDRAMRasN = 1
          , sDRAMCasN = 1
          , sDRAMBa = 0
          }

-- | MRS config:
--     - Full page burst
--     - sequential wrapping
--     - CL according to type
getModeReg
  :: forall (d :: Domain)
            (s :: SDRAMSpec)
   . KnownSDRAMSpec d s
  => BitVector (SDRAMAddrWidth d s)
getModeReg = case snatToNum @(Unsigned 2) (SNat @(SDRAMCL d s)) of
              2 -> 0b0100111
              3 -> 0b0110111
              _ -> errorX "Unexpected CL latency."

sDRAMCmdNoOp, sDRAMCmdMRS, sDRAMCmdAutoRefresh, sDRAMCmdBurstStop
  :: forall (d :: Domain)
            (s :: SDRAMSpec)
   . KnownSDRAMSpec d s
  => SDRAMCmd d s
sDRAMCmdNoOp
  = SDRAMCmd
      { sDRAMA = 0
      , sDRAMCKE = 1
      , sDRAMCSN = 0
      , sDRAMLDQM = 1
      , sDRAMUDQM = 1
      , sDRAMWeN = 1
      , sDRAMRasN = 1
      , sDRAMCasN = 1
      , sDRAMBa = 0
      }
sDRAMCmdMRS
  = SDRAMCmd
      { sDRAMA = getModeReg @d @s
      , sDRAMCKE = 1
      , sDRAMCSN = 0
      , sDRAMLDQM = 0
      , sDRAMUDQM = 0
      , sDRAMWeN = 0
      , sDRAMRasN = 0
      , sDRAMCasN = 0
      , sDRAMBa = 0
      }
sDRAMCmdAutoRefresh
  = SDRAMCmd
      { sDRAMA = 0
      , sDRAMCKE = 1
      , sDRAMCSN = 0
      , sDRAMLDQM = 0
      , sDRAMUDQM = 0
      , sDRAMWeN = 1
      , sDRAMRasN = 0
      , sDRAMCasN = 0
      , sDRAMBa = 0
      }
sDRAMCmdBurstStop
  = SDRAMCmd
      { sDRAMA = 0
      , sDRAMCKE = 1
      , sDRAMCSN = 0
      , sDRAMLDQM = 0
      , sDRAMUDQM = 0
      , sDRAMWeN = 0
      , sDRAMRasN = 1
      , sDRAMCasN = 1
      , sDRAMBa = 0
      }

sDRAMCmdBankActive
  :: forall (d :: Domain)
            (s :: SDRAMSpec)
   . KnownSDRAMSpec d s
  => Index (SDRAMBanks d s)
  -> Index (SDRAMRows d s)
  -> SDRAMCmd d s
sDRAMCmdBankActive bank row
  = SDRAMCmd
      { sDRAMA = resize $ pack row
      , sDRAMCKE = 1
      , sDRAMCSN = 0
      , sDRAMLDQM = 0
      , sDRAMUDQM = 0
      , sDRAMWeN = 1
      , sDRAMRasN = 0
      , sDRAMCasN = 1
      , sDRAMBa = pack bank
      }

sDRAMCmdWrite, sDRAMCmdRead
  :: forall (d :: Domain)
            (s :: SDRAMSpec)
   . KnownSDRAMSpec d s
  => Index (SDRAMBanks d s)
  -> Index (SDRAMCols d s)
  -> SDRAMCmd d s
sDRAMCmdWrite bank cols
  = SDRAMCmd
      { sDRAMA = resize $ pack cols
      , sDRAMCKE = 1
      , sDRAMCSN = 0
      , sDRAMLDQM = 0
      , sDRAMUDQM = 0
      , sDRAMWeN = 0
      , sDRAMRasN = 1
      , sDRAMCasN = 0
      , sDRAMBa = pack bank
      }
sDRAMCmdRead bank cols
  = SDRAMCmd
      { sDRAMA = resize $ pack cols
      , sDRAMCKE = 0
      , sDRAMCSN = 0
      , sDRAMLDQM = 0
      , sDRAMUDQM = 0
      , sDRAMWeN = 1
      , sDRAMRasN = 1
      , sDRAMCasN = 0
      , sDRAMBa = pack bank
      }

sDRAMCmdPreCharge
  :: forall (d :: Domain)
            (s :: SDRAMSpec)
   . KnownSDRAMSpec d s
  => Maybe (Index (SDRAMBanks d s))
  -> SDRAMCmd d s
sDRAMCmdPreCharge bankM
  = let (addr, ba) = case bankM of
                       Just b  -> (0, b)
                       Nothing -> (maxBound, 0)
    in SDRAMCmd
         { sDRAMA = addr
         , sDRAMCKE = 1
         , sDRAMCSN = 0
         , sDRAMLDQM = 0
         , sDRAMUDQM = 0
         , sDRAMWeN = 0
         , sDRAMRasN = 0
         , sDRAMCasN = 1
         , sDRAMBa = pack ba
         }
