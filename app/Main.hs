-- | Haskell language pragma
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
-- {-# LANGUAGE DuplicateRecordFields #-}

-- | Haskell module declaration
module Main where

-- | Miso framework import
import Miso 
import Miso.String 
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Control.Lens as Lens

-- | Type synonym for an application model
data Model = Model {
    _value :: Int
  , _color :: MisoString
  , _todoList :: [ItemModel]
  , _todoInput :: TodoInput
} deriving (Show, Eq)

data ItemModel = ItemModel {
    _imTitle :: MisoString
  , _imContent :: MisoString
} deriving (Show, Eq)

data TodoInput = TodoInput {
    _tiTitle :: InputState
  , _tiContent :: InputState
} deriving (Show, Eq)
-- type TodoInput = Map TodoField InputState

data InputState = 
    NotEnter
  | Entering MisoString
  | Error MisoString MisoString
  deriving (Show, Eq)

-- data TodoField = 
--     Title
--   | Content
--   deriving (Show, Eq, Ord)



-- | Sum type for application events
data FieldModifier = 
  FieldModifier (TodoInput -> InputState)

instance Show FieldModifier where
  show _ = "Field Modifier"

instance Eq FieldModifier where
  (==) _ _ = False


    -- deriving (Show, Eq)
data Action
  = AddOne
  | SubtractOne
  | NoOp
  | SayHelloWorld
  | ChangeColorGreen
  | CreateNewTask
  | Input ((InputState -> Lens.Identity InputState) -> TodoInput -> Lens.Identity TodoInput) MisoString

instance Show Action where
  show _ = "Action"
instance Eq Action where
  (==) _ _ = False

$(Lens.makeLenses ''TodoInput)

todoItemList :: [ItemModel]
todoItemList = 
  [ ItemModel {_imTitle = "Task 1", _imContent = "Content 1"}
  , ItemModel {_imTitle = "Task 2", _imContent = "Content 2"}
  ]
-- | Entry point for a miso application
main :: IO ()
main = startApp App {..}
  where
    initialAction = SayHelloWorld -- initial action to be executed on application load
    model         = Model {
                    _value = 0
                  , _color = "blue"
                  , _todoList = todoItemList
                  , _todoInput = TodoInput {_tiTitle = NotEnter, _tiContent = NotEnter}
                  }             -- initial model
    update        = updateModel   -- update function
    view          = viewModel     -- view function
    events        = defaultEvents -- default delegated events
    subs          = []            -- empty subscription list
    mountPoint    = Nothing       -- mount point for application (Nothing defaults to 'body')

-- | Updates model, optionally introduces side effects
updateModel :: Action -> Model -> Effect Action Model
updateModel action model = 
  case action of 
    AddOne -> noEff $ model{_value = (_value model) + 1}
    SubtractOne -> noEff $ model{_value = (_value model) - 1}
    NoOp -> noEff model
    SayHelloWorld -> model <# do
      putStrLn "Hello World"
      print $ 1 + 3
      pure NoOp
    ChangeColorGreen -> noEff $ model{_color = "green"}
    CreateNewTask -> noEff $ model{_todoList = (ItemModel "test" "test") : (_todoList model)}
    Input func s ->
      noEff model {_todoInput = newTodoInput}
        where
          -- func = _tiTitle
          newTodoInput = Lens.set func (Entering s) (_todoInput model) 
      -- case field of 
      --   Title -> case model of
      --     Nothing -> model{_todoInput}
-- | Constructs a virtual DOM from a model
viewModel :: Model -> View Action
viewModel x = div_ [] [
    h1_ [headingStyle] [text "this shit took avery long time"]
  , h2_ [] [text "Nis ker chea khgnom rerng perng chea jomhor"]
  , div_ [] [text "This is the body 2"]
  , p_ [style_ $ Map.singleton "background-color" (_color x)] [text "This is some paragraph that I want to display"]
  , button_ [onClick ChangeColorGreen][text "Change color to green"]
  , addNewTask
  , div_ [] $ fmap todoItemView (_todoList x)
  , button_ [onClick CreateNewTask][text "Submit"]
  , text $ ms $ show x
  ] where 
    headingStyle = style_ $ Map.fromList [("background-color", "blue"), ("color", "white")]
  
  
todoItemView :: ItemModel -> View Action 
todoItemView item = 
  div_ [] [
    h3_ [][text $ _imTitle item]
  , p_ [][text $ _imContent item]
  ]
  
addNewTask :: View Action  
addNewTask = 
  div_ [][ 
    input_ [styleBlock, placeholder_ "Enter new item detail here", onInput (Input tiTitle) ] [] 
  , textarea_ [styleBlock, onInput (Input tiContent)][] 
  ]

-- styleBlock :: Attribute Action
styleBlock = style_ $ Map.singleton "display" "block"
--   div_ [] [
--    button_ [ onClick AddOne ] [ text "+" ]
--  , text (ms x)
--  , button_ [ onClick SubtractOne ] [ text "-" ]
--  ]