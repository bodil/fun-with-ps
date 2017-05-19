module Bounds where

import Prelude

type Bounds = { x1 :: Number, y1 :: Number, x2 :: Number, y2 :: Number }

bounds :: Number → Number → Bounds
bounds x y = { x1: x + 24.0, y1: y + 24.0, x2: x + 104.0, y2: y + 104.0 }

dogBounds :: Number → Number → Bounds
dogBounds x y = { x1: x + 96.0, y1: y + 24.0, x2: x + 256.0, y2: y + 90.0 }

intersects :: Bounds → Bounds → Boolean
intersects b1 b2 =
  not (b2.x1 > b1.x2 || b2.x2 < b1.x1 || b2.y1 > b1.y2 || b2.y2 < b1.y1)
