module Component where

import           Data.Maybe                   (Maybe (..))
import           Effect.Aff                   (Aff)
import           Halogen                      as H
import           Halogen.HTML                 as HH
import           Halogen.HTML.Events          as HE
import           Halogen.HTML.Properties      as HP
import           Affjax                       as AX
import           Affjax.ResponseFormat
import           Prelude

type State = { loading :: Boolean
             , username :: String
             , result :: Maybe String
             }

data Query a
  = SetUsername String a
  | MakeRequest a

ui :: H.Component HH.HTML Query Unit Void Aff
ui =
  H.component { initialState: const initialState
              , render, eval
              , receiver: const Nothing
              }

initialState :: State
initialState = { loading: true
               , username: ""
               , result: Nothing
               }

render :: State -> H.ComponentHTML Query
render st =
  HH.form_ $
    [ HH.h1_ [ HH.text "Lookup Github user" ]
    ]

eval ::  Query ~> H.ComponentDSL State Query Void Aff
eval (SetUsername username next) = do
    H.modify_ (_ { username = username, result = Nothing })
    pure next
eval (MakeRequest next) = do
    H.modify_ (_ { username = "request", result = Nothing })
    pure next
eval (MakeRequest next) = do
    username <- H.gets _.username
    H.modify_ (_ { loading = true })
    response <- H.liftAff $ AX.get json ("https://api.github.com/users/" <> username)
    H.modify_ (_ { loading = false, result = Just response.response })
    pure next
