{-# LANGUAGE BlockArguments, RecursiveDo #-}
module TrafficJam.Input where

import TrafficJam
import Z80
import Z80.Utils

-- dispatchInput :: Location -> Location -> Location -> Location -> Z80ASM
-- dispatchInput north south east west = skippable \end -> mdo
--     ld A [0x3adf]
--     cp 0xff
--     jp Z isReleased

--     ld C A
--     ld A [released]
--     cp 0
--     jp Z end -- Avoid held keys repeating
--     ldVia A [released] 0
--     ld A C

--     -- Check "i"
--     rra
--     rra
--     jp NC north
--     -- Check "j"
--     rra
--     jp NC west
--     -- Check "k"
--     rra
--     jp NC south
--     -- Check "l"
--     rra
--     jp NC east
--     jp end

--     isReleased <- labelled do
--         ldVia A [released] 1
--         jp end


--     released <- labelled $ db [1]
--     pure ()
