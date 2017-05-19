module Main where

import Prelude hiding (div, top)
import Bounds (bounds, dogBounds, intersects)
import CSS (left, top, px)
import Control.Monad.Eff (Eff)
import Data.Array ((..), any)
import Data.Traversable (for_)
import Data.Int (toNumber)
import Data.Monoid (mempty)
import DOM (DOM)
import Pux (EffModel, noEffects, CoreEffects, App, start)
import Pux.DOM.Events (DOMEvent)
import Pux.DOM.HTML (HTML)
import Pux.DOM.HTML.Attributes (style)
import Pux.Renderer.React (renderToDOM)
import Signal ((~>), sampleOn, dropRepeats)
import Signal.DOM (keyPressed)
import Signal.Time (every)
import Text.Smolder.HTML (div)
import Text.Smolder.HTML.Attributes (className)
import Text.Smolder.Markup ((!))



type Object = {
  css :: String,
  x :: Number,
  y :: Number,
  vx :: Number,
  vy :: Number
  }

type State = {
  dogs :: Object,
  obstacles :: Array Object,
  gameOver :: Boolean
  }

newDogs :: Object
newDogs = { css: "dogs", x: 32.0, y: 192.0, vx: 0.0, vy: 0.0 }

newObstacle :: Int → Object
newObstacle n = { css: "obstacle o" <> show n,
                  x: 512.0 + 256.0 * (toNumber n), y: 128.0 * (toNumber n),
                  vx: -0.2 * (toNumber (4 - n)), vy: 0.0 }

initialState :: State
initialState = {
  dogs: newDogs,
  obstacles: map newObstacle (0..3),
  gameOver: false
  }



data Event = Nope
           | Up
           | Down



foldp :: ∀ fx. Event → State → EffModel State Event fx
foldp ev st = noEffects $ gameOver $ updatePhysics $ updateDogs ev st

collidedWith :: Object → Object → Boolean
collidedWith dogs obs = intersects (dogBounds dogs.x dogs.y) (bounds obs.x obs.y)

gameOver :: State → State
gameOver st@{gameOver: false} | any (collidedWith st.dogs) st.obstacles =
  st { gameOver = true }
gameOver st@{gameOver: false} = st
gameOver st@{gameOver: true} =
  st { dogs = st.dogs { vx = 0.0,
                        vy = st.dogs.vy + 0.4,
                        css = "dogs gameover" } }

physics :: Object → Object
physics o = o { x = o.x + o.vx, y = o.y + o.vy }

updateDogs :: Event → State → State
updateDogs Up st@{dogs} = st { dogs = dogs { vy = dogs.vy - 0.2 } }
updateDogs Down st@{dogs} = st { dogs = dogs { vy = dogs.vy + 0.2 } }
updateDogs Nope st = st

updatePhysics :: State → State
updatePhysics st = st {
  dogs = physics st.dogs,
  obstacles = map physics st.obstacles
  }



renderObject :: Object → HTML Event
renderObject o =
  div ! className o.css
  ! style do
      left (o.x # px)
      top (o.y # px)
  $ mempty

view :: State → HTML Event
view st =
  div ! className "app" $ do
    renderObject st.dogs
    for_ st.obstacles renderObject





main :: String → State → Eff (CoreEffects (dom :: DOM)) (App (DOMEvent → Event) Event State)
main url state = do
  -- | Set up a keyboard event stream.
  upKey ← keyPressed 87 -- "w"
  downKey ← keyPressed 83 -- "s"
  let upStream = dropRepeats upKey ~> if _ then Up else Nope
      downStream = dropRepeats downKey ~> if _ then Down else Nope
      keyboardStream = sampleOn (every 20.0) (upStream <> downStream)

  -- | Start the app.
  app ← start {
    initialState: state,
    view: view,
    foldp: foldp,
    inputs: [keyboardStream]
    }

  -- | Render to the DOM
  renderToDOM "#app" app.markup app.input

  -- | Return app to be used for hot reloading logic in support/client.entry.js
  pure app
