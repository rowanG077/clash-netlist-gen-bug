{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Example.SDRAM.Core where

import Clash.Prelude

import Example.Counter
import Example.SDRAM.Types
import Example.SDRAM.Command
import GHC.Stack
import Data.Maybe

-- | by taking into account the time it takes to do an operation
--   we ensure we never violate the refresh timing
type REFICounter (d :: Domain) (s :: SDRAMSpec)
  = CounterOverflow (SDRAMtREFI d s - SDRAMtOp d s) (SDRAMtOp d s)

data SDRAMState (d :: Domain) (s :: SDRAMSpec)
   = SDRAMInitIdle
       { sDRAMIdleCounter :: Counter (SDRAMtStartupDelay d s)
       }
   | SDRAMInitPrecharge
       { sDRAMInitPrechargeCounter :: Counter (SDRAMtRP d s)
       }
   | SDRAMInitRefresh
       { sDRAMWaitRefreshCounter :: Counter (SDRAMtRFC d s)
       , sDRAMInitRefreshIdx :: Counter (SDRAMStartupRefreshes d s)
       }
   | SDRAMInitLoadModeReg
       { sDRAMRefreshCounter :: REFICounter d s
       , sDRAMInitLoadModeRegCounter :: Counter (SDRAMtMRD d s)
       }
   | SDRAMIdle
       { sDRAMRefreshCounter :: REFICounter d s
       }
   | SDRAMRefresh
       { sDRAMRefreshCounter :: REFICounter d s
       , sDRAMWaitRefreshCounter :: Counter (SDRAMtRFC d s)
       }
   | SDRAMActive
       { sDRAMRefreshCounter :: REFICounter d s
       , sDRAMSelectedBank :: Index (SDRAMBanks d s)
       , sDRAMActiveCounter :: Counter (SDRAMtRCD d s)
       , sDRAMIsWriteReq :: Bool
       }
   | SDRAMRead
       { sDRAMRefreshCounter :: REFICounter d s
       , sDRAMSelectedBank :: Index (SDRAMBanks d s)
       , sDRAMReadCounter :: Counter (SDRAMCols d s)
       }
   | SDRAMWrite
       { sDRAMRefreshCounter :: REFICounter d s
       , sDRAMSelectedBank :: Index (SDRAMBanks d s)
       , sDRAMWriteCounter :: Counter (SDRAMCols d s - 1 + SDRAMtBDL d s)
       }
   | SDRAMWritePrechargeWait
       { sDRAMRefreshCounter :: REFICounter d s
       , sDRAMSelectedBank :: Index (SDRAMBanks d s)
       , sDRAMPrechargeWaitCounter :: Counter (SDRAMtWritePrechargeWait d s)
       }
   | SDRAMPrecharge
       { sDRAMRefreshCounter :: REFICounter d s
       -- Since we always go to idle after this state we can cheat and save a
       -- a cycle since it will always be one cycle in idle doing nothing.
       , sDRAMPrechargeCounter :: Counter (Max 1 (SDRAMtRP d s - 1))
       }

deriving instance KnownSDRAMSpec d s => Show (SDRAMState d s)
deriving instance KnownSDRAMSpec d s => Generic (SDRAMState d s)
deriving instance KnownSDRAMSpec d s => NFDataX (SDRAMState d s)

addrDecoder
  :: forall (d :: Domain)
            (s :: SDRAMSpec)
   . HasCallStack
  => KnownSDRAMSpec d s
  => Index (SDRAMBanks d s * SDRAMRows d s)
  -> (Index (SDRAMBanks d s), Index (SDRAMRows d s))
addrDecoder addr = let (ba, rows) = splitAt (SNat @(SDRAMBankWidth d s)) (bv2v $ resize $ pack addr)
                   in (bitCoerce ba, bitCoerce rows)

sDRAMCoreT
  :: forall (d :: Domain)
            (s :: SDRAMSpec)
   . HasCallStack
  => KnownSDRAMSpec d s
  => SDRAMState d s
  -> Maybe (SDRAMRequest d s)
  -> ( SDRAMState d s
     , (SDRAMCmd d s, Bool, Bool)
     )
sDRAMCoreT SDRAMInitIdle{..} _ = (st, (cmd, False, False))
  where
    done = hasOverflown sDRAMIdleCounter
    (st, cmd) | done
              = (SDRAMInitPrecharge mkCounter, sDRAMCmdPreCharge Nothing)
              | otherwise
              = (SDRAMInitIdle (tick sDRAMIdleCounter), sDRAMCmdNoOp)

sDRAMCoreT SDRAMInitPrecharge{..} _ = (st, (cmd, False, False))
  where
    done = hasOverflown sDRAMInitPrechargeCounter
    (st, cmd) | done
              = (SDRAMInitRefresh mkCounter mkCounter, sDRAMCmdAutoRefresh)
              | otherwise
              = (SDRAMInitPrecharge (tick sDRAMInitPrechargeCounter), sDRAMCmdNoOp)

sDRAMCoreT SDRAMInitRefresh{..} _ = (st, (cmd, False, False))
  where
    lastRefresh = hasOverflown sDRAMInitRefreshIdx
    nextRefresh = hasOverflown sDRAMWaitRefreshCounter

    nextRefreshCounter = tick sDRAMWaitRefreshCounter
    nextIdxCounter = tick sDRAMInitRefreshIdx

    newRefiCounter = mkCounter

    (st, cmd) | lastRefresh && nextRefresh
              = (SDRAMInitLoadModeReg newRefiCounter mkCounter, sDRAMCmdMRS)
              | nextRefresh
              = (SDRAMInitRefresh mkCounter nextIdxCounter, sDRAMCmdAutoRefresh)
              | otherwise
              = (SDRAMInitRefresh nextRefreshCounter sDRAMInitRefreshIdx, sDRAMCmdNoOp)

sDRAMCoreT SDRAMInitLoadModeReg{..} _ = (st, (sDRAMCmdNoOp, False, False))
  where
    done = hasOverflown sDRAMInitLoadModeRegCounter
    nextRefreshCounter = tick sDRAMRefreshCounter
    nextRegCounter = tick sDRAMInitLoadModeRegCounter

    st | done
       = SDRAMIdle nextRefreshCounter
       | otherwise
       = SDRAMInitLoadModeReg nextRefreshCounter nextRegCounter

sDRAMCoreT SDRAMIdle{..} inp = (st, (cmd, False, txAck))
  where
    gotoRefresh = hasOverflown sDRAMRefreshCounter

    isTx SDRAMWriteReq {} = True
    isTx SDRAMReadReq {} = False

    (bank, row) = addrDecoder @d @s (_sDRAMReqAddr $ fromJustX inp)
    tx = isTx $ fromJustX inp

    nextRefreshCounter = tick sDRAMRefreshCounter

    (st, cmd, txAck) | gotoRefresh
                     = (SDRAMRefresh mkCounter mkCounter, sDRAMCmdAutoRefresh, False)
                     | isJust inp
                     = (SDRAMActive nextRefreshCounter bank mkCounter tx, sDRAMCmdBankActive bank row, tx)
                     | otherwise
                     = (SDRAMIdle nextRefreshCounter, sDRAMCmdNoOp, False)

sDRAMCoreT SDRAMRefresh{..} _ = (st, (sDRAMCmdNoOp, False, False))
  where
    done = hasOverflown sDRAMWaitRefreshCounter

    nextRefreshCounter = tick sDRAMRefreshCounter
    nextWaitRefreshCounter = tick sDRAMWaitRefreshCounter

    st | done
       = SDRAMIdle nextRefreshCounter
       | otherwise
       = SDRAMRefresh nextRefreshCounter nextWaitRefreshCounter

sDRAMCoreT SDRAMActive{..} _ = (st, (cmd, rxAck, False))
  where
    done = hasOverflown sDRAMActiveCounter
    nextRefreshCounter = tick sDRAMRefreshCounter
    nextActiveCounter = tick sDRAMActiveCounter

    rxAck = done && not sDRAMIsWriteReq

    (st, cmd)
      | rxAck
      = (SDRAMRead nextRefreshCounter sDRAMSelectedBank mkCounter, sDRAMCmdRead sDRAMSelectedBank 0)
      | done
      = (SDRAMWrite nextRefreshCounter sDRAMSelectedBank mkCounter, sDRAMCmdWrite sDRAMSelectedBank 0) 
      | otherwise
      = (SDRAMActive nextRefreshCounter sDRAMSelectedBank nextActiveCounter sDRAMIsWriteReq, sDRAMCmdNoOp)

sDRAMCoreT SDRAMRead{..} _ = (st, (cmd, False, False))
  where
    done = hasOverflown sDRAMReadCounter
    nextRefreshCounter = tick sDRAMRefreshCounter
    nextReadCounter = tick sDRAMReadCounter

    (st, cmd) | done
              = case compareSNat (SNat @(SDRAMtRP d s)) d1 of
                  SNatLE -> (SDRAMIdle nextRefreshCounter, sDRAMCmdPreCharge (Just sDRAMSelectedBank))
                  SNatGT -> (SDRAMPrecharge nextRefreshCounter mkCounter, sDRAMCmdPreCharge (Just sDRAMSelectedBank))
              | otherwise
              = (SDRAMRead nextRefreshCounter sDRAMSelectedBank nextReadCounter, sDRAMCmdNoOp)

sDRAMCoreT SDRAMWrite{..} _ = (st, (cmd, False, False))
  where
    done = hasOverflown sDRAMWriteCounter
    nextRefreshCounter = tick sDRAMRefreshCounter
    nextWriteCounter = tick sDRAMWriteCounter

    (st, cmd) | done
              = (SDRAMWritePrechargeWait nextRefreshCounter sDRAMSelectedBank mkCounter, sDRAMCmdBurstStop)
              | otherwise
              = (SDRAMWrite nextRefreshCounter sDRAMSelectedBank nextWriteCounter, sDRAMCmdNoOp)

sDRAMCoreT SDRAMWritePrechargeWait{..} _ = (st, (cmd, False, False))
  where
    done = hasOverflown sDRAMPrechargeWaitCounter
    nextRefreshCounter = tick sDRAMRefreshCounter
    nextPrechargeWaitCounter = tick sDRAMPrechargeWaitCounter

    (st, cmd) | done
              = case compareSNat (SNat @(SDRAMtRP d s)) d1 of
                  SNatLE -> (SDRAMIdle nextRefreshCounter, sDRAMCmdPreCharge (Just sDRAMSelectedBank))
                  SNatGT -> (SDRAMPrecharge nextRefreshCounter mkCounter, sDRAMCmdPreCharge (Just sDRAMSelectedBank))
              | otherwise
              = (SDRAMWritePrechargeWait nextRefreshCounter sDRAMSelectedBank nextPrechargeWaitCounter, sDRAMCmdNoOp)

sDRAMCoreT SDRAMPrecharge{..} _ = (st, (sDRAMCmdNoOp, False, False))
  where
    done = hasOverflown sDRAMPrechargeCounter
    nextRefreshCounter = tick sDRAMRefreshCounter
    nextPrechargeCounter = tick sDRAMPrechargeCounter

    st | done = SDRAMIdle nextRefreshCounter
       | otherwise = SDRAMPrecharge nextRefreshCounter nextPrechargeCounter
