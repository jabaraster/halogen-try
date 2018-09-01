module Component where

import           Prelude

import           Affjax                       as AX
import           Affjax.ResponseFormat        as RF

import           Data.Either                  (Either(..))
import           Data.Maybe                   (Maybe (..))

import           Effect.Aff                   (Aff)

import           Halogen                      as H
import           Halogen.HTML                 as HH
import           Halogen.HTML.Events          as HE
import           Halogen.HTML.Properties      as HP

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
initialState = { loading: false
               , username: ""
               , result: Nothing
               }

render :: State -> H.ComponentHTML Query
render st =
    HH.form_ $
      [ HH.h1_ [ HH.text "Lookup GitHub user" ]
      , HH.label_
          [ HH.div_ [ HH.text "Enter username:" ]
          , HH.input
              [ HP.value st.username
              , HE.onValueInput (HE.input SetUsername)
              ]
          ]
      , HH.button
          [ HP.disabled st.loading
          , HP.type_ HP.ButtonButton
          , HE.onClick (HE.input_ MakeRequest)
          ]
          [ HH.text "Fetch info" ]
      , HH.p_
          [ HH.text (if st.loading then "Working..." else "") ]
      , HH.div_
          case st.result of
            Nothing -> []
            Just res ->
              [ HH.h2_
                  [ HH.text "Response:" ]
              , HH.pre_
                  [ HH.code_ [ HH.text res ] ]
              ]
      ]

eval ::  Query ~> H.ComponentDSL State Query Void Aff
eval (SetUsername username next) = do
    H.modify_ (_ { username = username, result = Nothing })
    pure next
eval (MakeRequest next) = do
    username <- H.gets _.username
    H.modify_ (_ { loading = true })
    res <- H.liftAff $ AX.get RF.string ("https://api.github.com/users/" <> username)
    case res.body of
        Left _     -> pure next
        Right json -> do
            H.modify_ (_ { loading = false, result = Just json })
            pure next
