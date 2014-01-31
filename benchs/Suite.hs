import Database.EventSafe.TypesBenchs
import Database.EventSafe.DiscPoolBenchs

import Criterion.Main

main :: IO ()
main = defaultMain
  [ typesBenchs
  , discPoolBenchs
  ]
