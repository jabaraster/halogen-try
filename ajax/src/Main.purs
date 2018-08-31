module Main where

import           Component           (ui)
import           Effect              (Effect)
import           Halogen.Aff         as HA
import           Halogen.VDom.Driver (runUI)
import           Prelude

main :: Effect Unit
main = HA.runHalogenAff $ do
    body <- HA.awaitBody
    runUI ui unit body
