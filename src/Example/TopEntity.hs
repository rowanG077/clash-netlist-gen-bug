module Example.TopEntity where

import Clash.Prelude

import Example.SDRAM.Core
import Example.SDRAM.Types
import Example.SDRAM.Modules

type RAMSpec = M12L16161A_5T 32
sDRAMModule :: SDRAMModule System RAMSpec
sDRAMModule = m12l16161a_5t @32 @System

sDRAMCoreT'
  :: forall (d :: Domain) (s :: SDRAMSpec)
   . SDRAMModule d s
  -> SDRAMState d s
  -> SDRAMState d s
sDRAMCoreT' SDRAMModule s = fst $ sDRAMCoreT s Nothing

topEntity :: Signal System (SDRAMState System RAMSpec) -> Signal System (SDRAMState System RAMSpec) 
topEntity s = fmap (sDRAMCoreT' m12l16161a_5t) s
