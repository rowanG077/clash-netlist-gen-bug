{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Example.SDRAM.Types where

import Clash.Prelude
import Data.Constraint
import Unsafe.Coerce (unsafeCoerce)
import Data.Type.Bool (If)

data SDRAMTiming
   = Cycles Nat
   | Picoseconds Nat

type family ExtractTiming (t :: SDRAMTiming) :: Nat where
  ExtractTiming ('Cycles n) = n
  ExtractTiming ('Picoseconds n) = n

type family SDRAMTimingCyclesRU (dom :: Domain) (timing :: SDRAMTiming) :: Nat where
  SDRAMTimingCyclesRU d ('Picoseconds s) = DivRU s (DomainPeriod d)
  SDRAMTimingCyclesRU _ ('Cycles c) = c

type family SDRAMTimingCycles (dom :: Domain) (timing :: SDRAMTiming) :: Nat where
  SDRAMTimingCycles d ('Picoseconds s) = Div s (DomainPeriod d)
  SDRAMTimingCycles _ ('Cycles c) = c

type family SDRAMTimingPicoseconds (dom :: Domain) (timing :: SDRAMTiming) :: Nat where
  SDRAMTimingPicoseconds _ ('Picoseconds s) = s
  SDRAMTimingPicoseconds d ('Cycles c) = c * DomainPeriod d

-- | The data width used by the SDRAM in Bits
newtype ParamSDRAMDataWidth = ParamSDRAMDataWidth Nat
-- | How many banks does the SDRAM module have
newtype ParamSDRAMBanks = ParamSDRAMBanks Nat
-- | How many rows does the SDRAM module have
newtype ParamSDRAMRows = ParamSDRAMRows Nat
-- | How many columns does the SDRAM module have
newtype ParamSDRAMCols = ParamSDRAMCols Nat
-- | How long do we need to wait until we can safely use the SDRAM
newtype ParamSDRAMtStartupDelay = ParamSDRAMtStartupDelay SDRAMTiming
-- | How many refreshes do we need to issue on startup
newtype ParamSDRAMStartupRefreshes = ParamSDRAMStartupRefreshes Nat
-- | What is the maximum amount of time we can wait between issuing two
--   auto refresh commands.
newtype ParamSDRAMtREFI = ParamSDRAMtREFI SDRAMTiming
-- | The minimum amount of time to idle after issueing a
--   Mode Register Set command. Alternate names: tRSC
newtype ParamSDRAMtMRD = ParamSDRAMtMRD SDRAMTiming
-- | The minimum amount of time between writing the last data and issueing a
--   Burst Stop command
newtype ParamSDRAMtBDL = ParamSDRAMtBDL SDRAMTiming
-- | The minimum amount of time to idle after issueing a Precharge command.
newtype ParamSDRAMtRP = ParamSDRAMtRP SDRAMTiming
-- | The minimum amount of time to idle after issueing a Row Activate command.
newtype ParamSDRAMtRCD = ParamSDRAMtRCD SDRAMTiming
-- | The minimum amount of time between writing the last data and issueing a
--   Precharge command. Alternate names: tDPL, tRWL, tRDL
newtype ParamSDRAMtWR = ParamSDRAMtWR SDRAMTiming
-- | The minimum amount of time to idle after issueing a Auto Refresh command.
newtype ParamSDRAMtRFC = ParamSDRAMtRFC SDRAMTiming
-- | The CL latency specified using a partition given clock period and two choices.
--   If DomainPeriod Nat <= DomainPeriod then first SDRAMTiming else second SDRAMTiming.
data ParamSDRAMCL = ParamSDRAMCL Nat SDRAMTiming SDRAMTiming
-- | The minimum clock period needed for the SDRAM to function.
newtype ParamSDRAMMinPeriod = ParamSDRAMMinPeriod Nat
-- | The maximum clock period needed for the SDRAM to function.
newtype ParamSDRAMMaxPeriod = ParamSDRAMMaxPeriod Nat

data SDRAMSpec
  = SDRAMSpec
      Symbol
      ParamSDRAMDataWidth
      ParamSDRAMBanks
      ParamSDRAMRows
      ParamSDRAMCols
      ParamSDRAMtStartupDelay
      ParamSDRAMStartupRefreshes
      ParamSDRAMtREFI
      ParamSDRAMtMRD
      ParamSDRAMtBDL
      ParamSDRAMtRP
      ParamSDRAMtRCD
      ParamSDRAMtWR
      ParamSDRAMtRFC
      ParamSDRAMCL
      ParamSDRAMMinPeriod
      ParamSDRAMMaxPeriod

-- Simple getters
type family GetParamSDRAMModuleName (s :: SDRAMSpec) :: Symbol where
  GetParamSDRAMModuleName ('SDRAMSpec moduleName _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _) = moduleName

type family GetParamSDRAMDataWidth (s :: SDRAMSpec) :: Nat where
  GetParamSDRAMDataWidth ('SDRAMSpec _ ('ParamSDRAMDataWidth v) _ _ _ _ _ _ _ _ _ _ _ _ _ _ _) = v

type family GetParamSDRAMBanks (s :: SDRAMSpec) :: Nat where
  GetParamSDRAMBanks ('SDRAMSpec _ _ ('ParamSDRAMBanks v) _ _ _ _ _ _ _ _ _ _ _ _ _ _) = v

type family GetParamSDRAMRows (s :: SDRAMSpec) :: Nat where
  GetParamSDRAMRows ('SDRAMSpec _ _ _ ('ParamSDRAMRows v) _ _ _ _ _ _ _ _ _ _ _ _ _) = v

type family GetParamSDRAMCols (s :: SDRAMSpec) :: Nat where
  GetParamSDRAMCols ('SDRAMSpec _ _ _ _ ('ParamSDRAMCols v) _ _ _ _ _ _ _ _ _ _ _ _) = v

type family GetParamSDRAMtStartupDelay (s :: SDRAMSpec) :: SDRAMTiming where
  GetParamSDRAMtStartupDelay ('SDRAMSpec _ _ _ _ _ ('ParamSDRAMtStartupDelay v) _ _ _ _ _ _ _ _ _ _ _) = v

type family GetParamSDRAMStartupRefreshes (s :: SDRAMSpec) :: Nat where
  GetParamSDRAMStartupRefreshes ('SDRAMSpec _ _ _ _ _ _ ('ParamSDRAMStartupRefreshes v) _ _ _ _ _ _ _ _ _ _) = v

type family GetParamSDRAMtREFI (s :: SDRAMSpec) :: SDRAMTiming where
  GetParamSDRAMtREFI ('SDRAMSpec _ _ _ _ _ _ _ ('ParamSDRAMtREFI v) _ _ _ _ _ _ _ _ _) = v

type family GetParamSDRAMtMRD (s :: SDRAMSpec) :: SDRAMTiming where
  GetParamSDRAMtMRD ('SDRAMSpec _ _ _ _ _ _ _ _ ('ParamSDRAMtMRD v) _ _ _ _ _ _ _ _) = v

type family GetParamSDRAMtBDL (s :: SDRAMSpec) :: SDRAMTiming where
  GetParamSDRAMtBDL ('SDRAMSpec _ _ _ _ _ _ _ _ _ ('ParamSDRAMtBDL v) _ _ _ _ _ _ _ ) = v

type family GetParamSDRAMtRP (s :: SDRAMSpec) :: SDRAMTiming where
  GetParamSDRAMtRP ('SDRAMSpec _ _ _ _ _ _ _ _ _ _ ('ParamSDRAMtRP v) _ _ _ _ _ _ ) = v

type family GetParamSDRAMtRCD (s :: SDRAMSpec) :: SDRAMTiming where
  GetParamSDRAMtRCD ('SDRAMSpec _ _ _ _ _ _ _ _ _ _ _ ('ParamSDRAMtRCD v) _ _ _ _ _) = v

type family GetParamSDRAMtWR (s :: SDRAMSpec) :: SDRAMTiming where
  GetParamSDRAMtWR ('SDRAMSpec _ _ _ _ _ _ _ _ _ _ _ _ ('ParamSDRAMtWR v) _ _ _ _ ) = v

type family GetParamSDRAMtRFC (s :: SDRAMSpec) :: SDRAMTiming where
  GetParamSDRAMtRFC ('SDRAMSpec _ _ _ _ _ _ _ _ _ _ _ _ _ ('ParamSDRAMtRFC v) _ _ _ ) = v

type family GetParamSDRAMCLPred (s :: SDRAMSpec) :: Nat where
  GetParamSDRAMCLPred ('SDRAMSpec _ _ _ _ _ _ _ _ _ _ _ _ _ _ ('ParamSDRAMCL p _ _) _ _) = p

type family GetParamSDRAMCLLeft (s :: SDRAMSpec) :: SDRAMTiming where
  GetParamSDRAMCLLeft ('SDRAMSpec _ _ _ _ _ _ _ _ _ _ _ _ _ _ ('ParamSDRAMCL _ l _) _ _) = l

type family GetParamSDRAMCLRight (s :: SDRAMSpec) :: SDRAMTiming where
  GetParamSDRAMCLRight ('SDRAMSpec _ _ _ _ _ _ _ _ _ _ _ _ _ _ ('ParamSDRAMCL _ _ r) _ _) = r

type family GetParamSDRAMMinPeriod (s :: SDRAMSpec) :: Nat where
  GetParamSDRAMMinPeriod ('SDRAMSpec _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ ('ParamSDRAMMinPeriod v) _) = v

type family GetParamSDRAMMaxPeriod (s :: SDRAMSpec) :: Nat where
  GetParamSDRAMMaxPeriod ('SDRAMSpec _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ ('ParamSDRAMMaxPeriod v)) = v

-- | Smart getters that only return nats in clock cycles
type SDRAMModuleName d s = GetParamSDRAMModuleName s
type SDRAMDataWidth d s = GetParamSDRAMDataWidth s
type SDRAMBanks d s = GetParamSDRAMBanks s
type SDRAMRows d s = GetParamSDRAMRows s
type SDRAMCols d s = GetParamSDRAMCols s
type SDRAMtStartupDelay d s = SDRAMTimingCyclesRU d (GetParamSDRAMtStartupDelay s)
type SDRAMStartupRefreshes d s = GetParamSDRAMStartupRefreshes s
type SDRAMtREFI d s = SDRAMTimingCycles d (GetParamSDRAMtREFI s)
type SDRAMtMRD d s = SDRAMTimingCyclesRU d (GetParamSDRAMtMRD s)
type SDRAMtBDL d s = SDRAMTimingCyclesRU d (GetParamSDRAMtBDL s)
type SDRAMtRP d s = SDRAMTimingCyclesRU d (GetParamSDRAMtRP s)
-- We wait at least 2 clock cycles so can pipeline
-- the core statemachine, the frontend and user code
type SDRAMtRCD d s = Max 2 (SDRAMTimingCyclesRU d (GetParamSDRAMtRCD s))
type SDRAMtWR d s = SDRAMTimingCyclesRU d (GetParamSDRAMtWR s)
type SDRAMtRFC d s = SDRAMTimingCyclesRU d (GetParamSDRAMtRFC s)
type SDRAMCLLeft d s = SDRAMTimingCyclesRU d (GetParamSDRAMCLLeft s)
type SDRAMCLRight d s = SDRAMTimingCyclesRU d (GetParamSDRAMCLRight s)
type SDRAMCL d s = If (GetParamSDRAMCLPred s <=? DomainPeriod d)
                     (SDRAMCLLeft d s)
                     (SDRAMCLRight d s)
type SDRAMMinPeriod d s = GetParamSDRAMMinPeriod s
type SDRAMMaxPeriod d s = GetParamSDRAMMaxPeriod s

type SDRAMRowAddrWidth (d :: Domain) (s :: SDRAMSpec) = CLog 2 (SDRAMRows d s)
type SDRAMColAddrWidth (d :: Domain) (s :: SDRAMSpec) = CLog 2 (SDRAMCols d s)
type SDRAMAddrWidth (d :: Domain) (s :: SDRAMSpec) = Max (SDRAMColAddrWidth d s) (SDRAMRowAddrWidth d s)
type SDRAMBankWidth (d :: Domain) (s :: SDRAMSpec) = CLog 2 (SDRAMBanks d s)

type SDRAMtReadAct (d :: Domain) (s :: SDRAMSpec)
  = SDRAMCols d s + (SDRAMtRP d s - 1)

-- After Activating a row  we write column data,
-- after writing we need to issue a manual Burst Stop because stopping a write
-- burst with a Precharge leads to writing undefined data for tWR cycles, unless
-- we mask the data out using DQM. However the PCB I use doesn't have DQM pins
-- connected so we cannot mask. Fortunately manually issueing a Burst Stop
-- doesn't require a mask. But that makes the State machine a little more complex
-- because we can issue a Precharge a minimum tWR cycles after the last data in.
-- But we can issue a Burst Stop a minimum of tBDL cycles after the last data in.
-- These times are overlapping that means naively we can issue a Precharge
-- (tWR - tBDL) cycles after sending the Burst Stop. tBDL may be >= tWR so it's
-- not so easy. That means we can issue a Precharge after (Max tBDL tWR) - tBDL?
-- No, because that may be 0. And we cannot issue 2 commands in the same clock cycle
-- So finally we get to (Max (tBDL + 1) tWR) - tBDL cycles.
type SDRAMtWriting (d :: Domain) (s :: SDRAMSpec)
  = SDRAMCols d s - 1 + SDRAMtBDL d s

type SDRAMtWritePrechargeWait (d :: Domain) (s :: SDRAMSpec) 
  = Max (SDRAMtBDL d s + 1) (SDRAMtWR d s) - SDRAMtBDL d s

type SDRAMtWriteAct (d :: Domain) (s :: SDRAMSpec) 
  = SDRAMtWriting d s
    + SDRAMtWritePrechargeWait d s
    + (SDRAMtRP d s - 1)

-- | The amount of cycles it takes to complete a read operation.
type SDRAMtRead (d :: Domain) (s :: SDRAMSpec)
  = SDRAMtRCD d s + SDRAMtReadAct d s

-- | The amount of cycles it takes to complete a write operation.
type SDRAMtWrite (d :: Domain) (s :: SDRAMSpec)
  = SDRAMtRCD d s + SDRAMtWriteAct d s

-- | The amount of cycles it takes to complete an operation.
--   Is in fact always equal to the time to write but this way it's more generic
--   if other things change.
type SDRAMtOp (d :: Domain) (s :: SDRAMSpec)
  = Max (SDRAMtRead d s) (SDRAMtWrite d s)

-- | Very large and scary constraint. But While defining a concrete
--   module in `SDRAM.Modules` almost constraints are trivially satisfiable
--   except for the constraints involving domain periods.
type KnownSDRAMSpecBase (d :: Domain) (s :: SDRAMSpec)
   = ( KnownSymbol (SDRAMModuleName d s)
     , KnownNat (SDRAMDataWidth d s)
     , 1 <= GetParamSDRAMBanks s
     , KnownNat (SDRAMBanks d s)
     , 1 <= GetParamSDRAMRows s
     , KnownNat (SDRAMRows d s)
     , 1 <= GetParamSDRAMCols s
     , KnownNat (SDRAMCols d s)
     , 1 <= (SDRAMBanks d s * SDRAMRows d s)
     , 1 <= ExtractTiming (GetParamSDRAMtStartupDelay s)
     , KnownNat (SDRAMtStartupDelay d s)
     , 1 <= GetParamSDRAMStartupRefreshes s
     , KnownNat (SDRAMStartupRefreshes d s)
     , 1 <= ExtractTiming (GetParamSDRAMtREFI s)
     , DomainPeriod d <= SDRAMTimingPicoseconds d (GetParamSDRAMtREFI s)
     , KnownNat (SDRAMtREFI d s)
     , 1 <= ExtractTiming (GetParamSDRAMtMRD s)
     , KnownNat (SDRAMtMRD d s)
     , 1 <= ExtractTiming (GetParamSDRAMtBDL s)
     , KnownNat (SDRAMtBDL d s)
     , 1 <= ExtractTiming (GetParamSDRAMtRP s)
     , KnownNat (SDRAMtRP d s)
     , 1 <= ExtractTiming (GetParamSDRAMtRCD s)
     , KnownNat (SDRAMtRCD d s)
     , 1 <= ExtractTiming (GetParamSDRAMtWR s)
     , KnownNat (SDRAMtWR d s)
     , 1 <= ExtractTiming (GetParamSDRAMtRFC s)
     , KnownNat (SDRAMtRFC d s)
     , 1 <= ExtractTiming (GetParamSDRAMCLLeft s)
     , 1 <= ExtractTiming (GetParamSDRAMCLRight s)
     , KnownNat (SDRAMCL d s)
     , KnownDomain d
     , 1 <= DomainPeriod d
     , 1 <= GetParamSDRAMMinPeriod s
     , GetParamSDRAMMinPeriod s <= GetParamSDRAMMaxPeriod s
     , GetParamSDRAMMinPeriod s <= DomainPeriod d
     , DomainPeriod d <= GetParamSDRAMMaxPeriod s
     , SDRAMtOp d s + 1 <= SDRAMtREFI d s
     )

-- | These constraints aren't inferable but we can prove them
-- manually using the constraints given in `KnownSDRAMSpecBase`
type KnownSDRAMSpec (d :: Domain) (s :: SDRAMSpec)
   = ( KnownSDRAMSpecBase d s
     , 1 <= SDRAMtStartupDelay d s
     , 1 <= SDRAMtREFI d s
     , 1 <= SDRAMtMRD d s
     , 1 <= SDRAMtBDL d s
     , 1 <= SDRAMtRP d s
     , 1 <= SDRAMtRCD d s
     , 2 <= SDRAMtRCD d s
     , 1 <= SDRAMtWR d s
     , 1 <= SDRAMtRFC d s
     , 1 <= SDRAMCL d s
     , 1 <= SDRAMtRead d s
     , 1 <= SDRAMtWrite d s
     , 1 <= SDRAMtOp d s
     , SDRAMtOp d s <= SDRAMtREFI d s
     , 1 <= SDRAMtREFI d s - SDRAMtOp d s
     -- Sub expr of SDRAMtOp
     , SDRAMtBDL d s <= Max (SDRAMtBDL d s + 1) (SDRAMtWR d s)
     )

data SDRAMModule (dom :: Domain) (spec :: SDRAMSpec) where
  SDRAMModule :: KnownSDRAMSpec dom spec => SDRAMModule dom spec

sDRAMtimingAxiomRU
  :: forall (dom :: Domain)
            (t :: SDRAMTiming)
   . (1 <= DomainPeriod dom, 1 <= ExtractTiming t)
  :- (1 <= SDRAMTimingCyclesRU dom t)
sDRAMtimingAxiomRU = Sub (unsafeCoerce (Dict :: Dict (a ~ a)))

sDRAMtimingAxiom
  :: forall (dom :: Domain)
            (t :: SDRAMTiming)
   . (DomainPeriod dom <= SDRAMTimingPicoseconds dom t)
  :- (1 <= SDRAMTimingCycles dom t)
sDRAMtimingAxiom = Sub (unsafeCoerce (Dict :: Dict (a ~ a)))

ifAxiom
  :: forall (n :: Nat)
            (c :: Bool)
            (p :: Nat)
            (k :: Nat)
   . (n <= p, n <= k) :- (n <= If c p k)
ifAxiom = Sub (unsafeCoerce (Dict :: Dict (a ~ a)))

maxAxiomLeft
  :: forall (n :: Nat)
            (p :: Nat)
            (k :: Nat)
   . (n <= p) :- (n <= Max p k)
maxAxiomLeft = Sub (unsafeCoerce (Dict :: Dict (a ~ a)))

sum2Axiom
  :: forall (n :: Nat)
            (p :: Nat)
            (q :: Nat)
   . (q <= n) :- (q <= n + p)
sum2Axiom = Sub (unsafeCoerce (Dict :: Dict (a ~ a)))

plusMonotoneAxiom
  :: forall (a :: Nat)
            (b :: Nat)
            (c :: Nat)
   . (a + b <= c) :- (a <= c)
plusMonotoneAxiom = Sub (unsafeCoerce (Dict :: Dict (a ~ a)))

applyMinus
  :: forall (a :: Nat)
            (b :: Nat)
            (c :: Nat)
   . (a + b <= c) :- (b <= c - a)
applyMinus = Sub (unsafeCoerce (Dict :: Dict (a ~ a)))

mkSDRAMModule 
  :: forall (d :: Domain)
            (s :: SDRAMSpec)
   . KnownSDRAMSpecBase d s
  => SDRAMModule d s
mkSDRAMModule = case lvl1 of 
                  Sub Dict -> case lvl2 of
                    Sub Dict -> case lvl3 of
                      Sub Dict -> SDRAMModule @d @s
  where
    lvl1 = tStartupDelay *** tREFI *** tMRD *** tBDL *** tRP *** tRCD1 *** tRCD2
            *** tWR *** tRFC *** clLeft *** clRight *** tRefiOp *** tRefiOp2
    lvl2 = ifCl *** tRead *** tWrite *** tWriteInner
    lvl3 = tOp

    tStartupDelay = sDRAMtimingAxiomRU @d @(GetParamSDRAMtStartupDelay s)
    tREFI = sDRAMtimingAxiom @d @(GetParamSDRAMtREFI s)
    tMRD = sDRAMtimingAxiomRU @d @(GetParamSDRAMtMRD s)
    tBDL = sDRAMtimingAxiomRU @d @(GetParamSDRAMtBDL s)
    tRP = sDRAMtimingAxiomRU @d @(GetParamSDRAMtRP s)
    tRCD1 = maxAxiomLeft @1 @3 @(SDRAMTimingCyclesRU d (GetParamSDRAMtRCD s))
    tRCD2 = maxAxiomLeft @2 @3 @(SDRAMTimingCyclesRU d (GetParamSDRAMtRCD s))
    tWR = sDRAMtimingAxiomRU @d @(GetParamSDRAMtWR s)
    tRFC = sDRAMtimingAxiomRU @d @(GetParamSDRAMtRFC s)

    clLeft = sDRAMtimingAxiomRU @d @(GetParamSDRAMCLLeft s)
    clRight = sDRAMtimingAxiomRU @d @(GetParamSDRAMCLRight s)
    ifCl = ifAxiom
             @1
             @(GetParamSDRAMCLPred s <=? DomainPeriod d)
             @(SDRAMCLLeft d s)
             @(SDRAMCLRight d s)

    tRead = sum2Axiom
              @(SDRAMtRCD d s)
              @(SDRAMtReadAct d s)
              @1
    tWrite = sum2Axiom
               @(SDRAMtRCD d s)
               @(SDRAMtWriteAct d s)
               @1

    tOp = maxAxiomLeft @1 @(SDRAMtRead d s) @(SDRAMtWrite d s)
    tWriteInner = maxAxiomLeft @(SDRAMtBDL d s) @(SDRAMtBDL d s + 1) @(SDRAMtWR d s)

    tRefiOp = plusMonotoneAxiom @(SDRAMtOp d s) @1 @(SDRAMtREFI d s)
    tRefiOp2 = applyMinus @(SDRAMtOp d s) @1 @(SDRAMtREFI d s)

type SDRAMAddr (d :: Domain) (s :: SDRAMSpec) = SDRAMBanks d s * SDRAMRows d s

data SDRAMRequest (d :: Domain)  (s :: SDRAMSpec)
   = SDRAMWriteReq
       { _sDRAMReqAddr :: Index (SDRAMAddr d s)
       }
   | SDRAMReadReq
       { _sDRAMReqAddr :: Index (SDRAMAddr d s)
       }

deriving instance KnownSDRAMSpec d s => Show (SDRAMRequest d s)
deriving instance KnownSDRAMSpec d s => Eq (SDRAMRequest d s)
deriving instance KnownSDRAMSpec d s => Generic (SDRAMRequest d s)
deriving instance KnownSDRAMSpec d s => NFDataX (SDRAMRequest d s)
deriving instance KnownSDRAMSpec d s => BitPack (SDRAMRequest d s)
