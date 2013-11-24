import Database.EventSafe.TypesBenchs

import Criterion.Main

main :: IO ()
main = defaultMain
  [ typesBenchs
  ]
