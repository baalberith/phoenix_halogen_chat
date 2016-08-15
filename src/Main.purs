-- module Main where

-- import Prelude
-- import Control.Monad.Eff (Eff)
-- import Control.Monad.Eff.Console (CONSOLE, log)

-- main :: forall e. Eff (console :: CONSOLE | e) Unit
-- main = do
--   log "Hello sailor!"

-- module Main where

-- import Prelude

-- import Control.Monad.Aff (Aff, later')
-- import Control.Monad.Eff (Eff)

-- import Halogen as H
-- import Halogen.HTML.Indexed as HH
-- import Halogen.HTML.Properties.Indexed as HP
-- import Halogen.Util (runHalogenAff, awaitBody)

-- type State = Int

-- initialState :: State
-- initialState = 0

-- data Query a = Tick a

-- ui :: forall g. H.Component State Query g
-- ui = H.component { render, eval }
--   where

--   render :: State -> H.ComponentHTML Query
--   render n =
--     HH.div_
--       [ HH.h1
--           [ HP.id_ "header" ]
--           [ HH.text "counter" ]
--       , HH.p_
--           [ HH.text (show n) ]
--       ]

--   eval :: Query ~> H.ComponentDSL State Query g
--   eval (Tick next) = do
--     H.modify (_ + 1)
--     pure next

-- -- | Run the app
-- main :: Eff (H.HalogenEffects ()) Unit
-- main = runHalogenAff do
--   body <- awaitBody
--   driver <- H.runUI ui initialState body
--   setInterval 1000 $ driver (H.action Tick)

-- setInterval :: forall e a. Int -> Aff e a -> Aff e Unit
-- setInterval ms a = later' ms $ do
--   a
--   setInterval ms a

module Main where

import Prelude

import Control.Coroutine (Producer, Consumer, consumer, runProcess, ($$))
import Control.Coroutine.Aff (produce)
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Var (($=))

import Data.Array as Array
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))

import Halogen as H
import Halogen.HTML.Indexed as HH
import Halogen.Util (runHalogenAff, awaitBody)

import WebSocket (WEBSOCKET, Connection(..), Message(..), URL(..), runMessageEvent, runMessage, newWebSocket)

----------------------------------------------------------------------------
-- Halogen component. This just displays a list of messages and has a query
-- to accept new messages.
----------------------------------------------------------------------------

type State = { messages :: Array String }

initialState :: State
initialState = { messages: [] }

data Query a = AddMessage String a

ui :: forall g. H.Component State Query g
ui = H.component { render, eval }
  where
  render :: State -> H.ComponentHTML Query
  render state =
    HH.ol_ $ map (\msg -> HH.li_ [ HH.text msg ]) state.messages

  eval :: Query ~> H.ComponentDSL State Query g
  eval (AddMessage msg next) = do
    H.modify \st -> { messages: st.messages `Array.snoc` msg }
    pure next

----------------------------------------------------------------------------
-- Websocket coroutine producer. This uses `purescript-aff-coroutines` to
-- create a producer of messages from a websocket.
----------------------------------------------------------------------------

wsProducer :: forall eff. Producer String (Aff (avar :: AVAR, err :: EXCEPTION, ws :: WEBSOCKET | eff)) Unit
wsProducer = produce \emit -> do
  Connection socket <- newWebSocket (URL "ws://echo.websocket.org") []

  -- This part is probably unnecessary in the real world, but it gives us 
  -- some messages to consume when using the echo service
  socket.onopen $= \event -> do
    socket.send (Message "hello")
    socket.send (Message "something")
    socket.send (Message "goodbye")

  socket.onmessage $= \event -> do
    emit $ Left $ runMessage (runMessageEvent event)

----------------------------------------------------------------------------
-- Coroutine consumer. This accepts a Halogen driver function and sends
-- `AddMessage` queries in when the coroutine consumes an input.
----------------------------------------------------------------------------

wsConsumer
  :: forall eff
   . (Query ~> Aff (H.HalogenEffects (ws :: WEBSOCKET | eff)))
  -> Consumer String (Aff (H.HalogenEffects (ws :: WEBSOCKET | eff))) Unit
wsConsumer driver = consumer \msg -> do
  driver $ H.action $ AddMessage msg
  pure Nothing

----------------------------------------------------------------------------
-- Normal Halogen-style `main`, the only addition is a use of `runProcess`
-- to connect the producer and consumer and start sending messages to the
-- Halogen component.
----------------------------------------------------------------------------

main :: forall eff. Eff (H.HalogenEffects (ws :: WEBSOCKET | eff)) Unit
main = runHalogenAff do
  body <- awaitBody
  driver <- H.runUI ui initialState body
  runProcess (wsProducer $$ wsConsumer driver)
  pure unit


