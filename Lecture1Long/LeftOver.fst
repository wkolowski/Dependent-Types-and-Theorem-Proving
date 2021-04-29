
(*

assume val readSecret : 

type atmState : Type = | Ready | CardInserted | Session

assume type atm : atmState -> Type


assume val initATM : () -> atm Ready
assume val shutDown : atm Ready -> ()

assume type card

assume val insertCard : card -> atm Ready -> atm CardInserted
assume val ejectCard  : atm CardInserted -> atm Ready

assume type pin

assume checkPIN : atm CardInserted -> (p : pin) -> atm (if 

assume type money
assume val message : string -> (#s : atmState) -> atm s -> atm s

assume val dispense : money -> atm Session -> 
getInput
*)