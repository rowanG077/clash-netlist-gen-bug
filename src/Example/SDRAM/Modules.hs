{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE FlexibleContexts #-}

module Example.SDRAM.Modules where
import Clash.Explicit.Prelude
import Example.SDRAM.Types

type SDRAMModuleDef dom spec
   = KnownDomain dom
  => 1 <= DomainPeriod dom
  => SDRAMMinPeriod dom spec <= DomainPeriod dom
  => DomainPeriod dom <= SDRAMMaxPeriod dom spec
  => DomainPeriod dom <= SDRAMTimingPicoseconds dom (GetParamSDRAMtREFI spec)
  => KnownNat (DomainPeriod dom)
  => KnownNat (SDRAMDataWidth dom spec)
  => SDRAMtOp dom spec + 1 <= SDRAMtREFI dom spec
  => SDRAMModule dom spec

-- | Specification for <https://www.esmt.com.tw/upload/pdf/ESMT/datasheets/M12L16161A(2R).pdf>
type M12L16161A_5T dataWidth = 'SDRAMSpec
    "M12L16161A_5T"
    ('ParamSDRAMDataWidth dataWidth)
    ('ParamSDRAMBanks 2)
    ('ParamSDRAMRows 2048)
    ('ParamSDRAMCols 256)
    ('ParamSDRAMtStartupDelay ('Picoseconds 200_000_000))
    ('ParamSDRAMStartupRefreshes 2)
    ('ParamSDRAMtREFI ('Picoseconds 15_600_000))
    ('ParamSDRAMtMRD ('Cycles 2))
    ('ParamSDRAMtBDL ('Cycles 1))
    ('ParamSDRAMtRP ('Picoseconds 15_000))
    ('ParamSDRAMtRCD ('Picoseconds 15_000))
    ('ParamSDRAMtWR ('Cycles 2))
    ('ParamSDRAMtRFC ('Picoseconds 55_000))
    ('ParamSDRAMCL 7000 ('Cycles 2) ('Cycles 3))
    ('ParamSDRAMMinPeriod 5000)
    ('ParamSDRAMMaxPeriod 1000000)

m12l16161a_5t
  :: forall dataWidth dom spec 
   . spec ~ M12L16161A_5T dataWidth
  => SDRAMModuleDef dom spec
m12l16161a_5t = mkSDRAMModule @dom @spec

-- | Specification for <https://www.esmt.com.tw/upload/pdf/ESMT/datasheets/M12L64322A(2S).pdf>
type M12L64322A_5TG2S dataWidth = 'SDRAMSpec
    "M12L64322A_5TG2S"
    ('ParamSDRAMDataWidth dataWidth)
    ('ParamSDRAMBanks 4)
    ('ParamSDRAMRows 2048)
    ('ParamSDRAMCols 256)
    ('ParamSDRAMtStartupDelay ('Picoseconds 200_000_000))
    ('ParamSDRAMStartupRefreshes 2)
    ('ParamSDRAMtREFI ('Picoseconds 15_600_000))
    ('ParamSDRAMtMRD ('Cycles 2))
    ('ParamSDRAMtBDL ('Cycles 1))
    ('ParamSDRAMtRP ('Picoseconds 15_000))
    ('ParamSDRAMtRCD ('Picoseconds 15_000))
    ('ParamSDRAMtWR ('Cycles 2))
    ('ParamSDRAMtRFC ('Picoseconds 55_000))
    ('ParamSDRAMCL 10000 ('Cycles 2) ('Cycles 3))
    ('ParamSDRAMMinPeriod 5000)
    ('ParamSDRAMMaxPeriod 1000000)

m12l64322a_5tg2s
  :: forall dataWidth dom spec 
   . spec ~ M12L64322A_5TG2S dataWidth
  => SDRAMModuleDef dom spec
m12l64322a_5tg2s = mkSDRAMModule @dom @spec

-- | Specification for <https://www.winbond.com/resource-files/w9816g6jh_a03.pdf>
type W9816G6JH_6 dataWidth = 'SDRAMSpec
    "W9816G6JH_6"
    ('ParamSDRAMDataWidth dataWidth)
    ('ParamSDRAMBanks 2)
    ('ParamSDRAMRows 2048)
    ('ParamSDRAMCols 256)
    ('ParamSDRAMtStartupDelay ('Picoseconds 200_000_000))
    ('ParamSDRAMStartupRefreshes 8)
    ('ParamSDRAMtREFI ('Picoseconds 15_600_000))
    ('ParamSDRAMtMRD ('Cycles 2))
    ('ParamSDRAMtBDL ('Cycles 1))
    ('ParamSDRAMtRP ('Picoseconds 18_000))
    ('ParamSDRAMtRCD ('Picoseconds 18_000))
    ('ParamSDRAMtWR ('Cycles 2))
    -- Is the same as tRC for this chip
    ('ParamSDRAMtRFC ('Picoseconds 60_000))
    ('ParamSDRAMCL 8000 ('Cycles 2) ('Cycles 3))
    ('ParamSDRAMMinPeriod 6000)
    ('ParamSDRAMMaxPeriod 1000000)

w9816g6jh_6
  :: forall dataWidth dom spec 
   . spec ~ W9816G6JH_6 dataWidth
  => SDRAMModuleDef dom spec
w9816g6jh_6 = mkSDRAMModule @dom @spec

-- | Specification for <https://www.etron.com/manager/uploads/EM636165_34_General%20.pdf>
type EM636165_6G dataWidth = 'SDRAMSpec
    "EM636165_6G"
    ('ParamSDRAMDataWidth dataWidth)
    ('ParamSDRAMBanks 2)
    ('ParamSDRAMRows 2048)
    ('ParamSDRAMCols 256)
    ('ParamSDRAMtStartupDelay ('Picoseconds 200_000_000))
    ('ParamSDRAMStartupRefreshes 2)
    ('ParamSDRAMtREFI ('Picoseconds 15_600_000))
    ('ParamSDRAMtMRD ('Cycles 1))
    ('ParamSDRAMtBDL ('Cycles 1))
    ('ParamSDRAMtRP ('Picoseconds 18_000))
    ('ParamSDRAMtRCD ('Picoseconds 18_000))
    ('ParamSDRAMtWR ('Cycles 2))
    -- Is the same as tRC for this chip
    ('ParamSDRAMtRFC ('Picoseconds 60_000))
    ('ParamSDRAMCL 7500 ('Cycles 2) ('Cycles 3))
    ('ParamSDRAMMinPeriod 6000)
    ('ParamSDRAMMaxPeriod 1000000)

em636165_6g
  :: forall dataWidth dom spec 
   . spec ~ EM636165_6G dataWidth
  => SDRAMModuleDef dom spec
em636165_6g = mkSDRAMModule @dom @spec
