module AsmGen where
import ASTGen

-- === Register Conventions === --
-- Note: For now, we use naive register allocation
-- r0 - zero register
-- r1 - return address/return value
-- r2 - stack pointer
-- r3 - binop left
-- r4 - binop right/result
-- r5,r6,r7 - temps

data AsmReg = R0 | R1 | R2 | R3 | R4 | R6 | R7 deriving Enum