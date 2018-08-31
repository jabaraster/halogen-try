module Component.List where

import           Component.Task          (TaskMessage (..), TaskQuery (..), task)
import           Data.Array              (filter, length, snoc)
import           Data.Map                as M
import           Data.Maybe              (Maybe (..), fromMaybe)
import           Halogen                 as H
import           Halogen.HTML            as HH
import           Halogen.HTML.Core       as HC
import           Halogen.HTML.Events     as HE
import           Halogen.HTML.Properties as HP
import           Model                   (List, TaskId, initialList, initialTask)
import           Prelude

data ListQuery a
  = NewTask a
  | AllDone a
  | HandleTaskMessage TaskId TaskMessage a

newtype TaskSlot = TaskSlot TaskId
derive instance eqTaskSlot :: Eq TaskSlot
derive instance ordTaskSlot :: Ord TaskSlot

list :: forall m. Applicative m => H.Component HH.HTML ListQuery Unit Void m
list =
    H.parentComponent { initialState: const initialList
                      , render, eval
                      , receiver: const Nothing
                      }
  where
    render :: List -> H.ParentHTML ListQuery TaskQuery TaskSlot m
    render st =
        HH.div
          [ HP.classes [ HC.ClassName "container" ] ]
          [ HH.h1_ [ HH.text "Todo list" ]
          , HH.p_
              [ HH.button
                  [ HE.onClick (HE.input_ NewTask) ]
                  [ HH.text "New Task" ]
              ]
          , HH.ul_ (map renderTask st.tasks)
          , HH.p_ [ HH.text $ show st.numCompleted <> " / " <> show (length st.tasks) <> " complete" ]
          , HH.button
              [ HE.onClick (HE.input_ AllDone) ]
              [ HH.text "All Done" ]
          ]

    renderTask :: TaskId -> H.ParentHTML ListQuery TaskQuery TaskSlot m
    renderTask taskId =
        HH.slot
          (TaskSlot taskId)
          (task initialTask)
          unit
          (HE.input (HandleTaskMessage taskId))

    eval :: ListQuery ~> H.ParentDSL List ListQuery TaskQuery TaskSlot Void m
    eval (NewTask next) = do
        H.modify_ addTask
        pure next
    eval (AllDone next) = do
        toggled <- H.queryAll (H.action (ToggleCompleted true))
        H.modify_ $ updateNumCompleted (const (M.size toggled))
        pure next
    eval (HandleTaskMessage p msg next) = do
        case msg of
          NotifyRemove -> do
            wasComplete <- H.query (TaskSlot p) (H.request IsCompleted)
            when (fromMaybe false wasComplete) $ H.modify_ $ updateNumCompleted (_ `sub` 1)
            H.modify_ (removeTask p)
          Toggled b ->
            H.modify_ $ updateNumCompleted (if b then (_ + 1) else (_ `sub` 1))
        pure next

addTask :: List -> List
addTask st = st { nextId = st.nextId + 1, tasks = st.tasks `snoc` st.nextId }

removeTask :: TaskId -> List -> List
removeTask taskId st = st { tasks = filter (_ /= taskId) st.tasks }

updateNumCompleted :: (Int -> Int) -> List -> List
updateNumCompleted f st = st { numCompleted = f st.numCompleted }
