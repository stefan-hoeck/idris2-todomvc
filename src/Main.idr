module Main

import Control.Monad.State
import Data.List
import Data.MSF.Trans
import Rhone.JS

%default total

--------------------------------------------------------------------------------
--          Model
--------------------------------------------------------------------------------

record Item where
  constructor MkItem
  id   : Nat
  todo : String
  done : Bool

data Ev = New | Clear | Abort Nat | Mark Bool
        | Edit Nat | Del Nat | Upd Nat | Toggle Nat Bool

record App where
  constructor MkApp
  id       : Nat
  items    : List Item
  newEl    : HTMLInputElement
  mainEl   : HTMLElement
  listEl   : HTMLElement
  footerEl : HTMLElement
  countEl  : HTMLElement
  clearEl  : HTMLButtonElement
  allEl    : HTMLInputElement

--------------------------------------------------------------------------------
--          View
--------------------------------------------------------------------------------

liId : Nat -> ElemRef HTMLLIElement
liId n = MkRef Li "item_\{show n}"

viewId : Nat -> ElemRef HTMLDivElement
viewId n = MkRef Div "view_\{show n}"

editId : Nat -> ElemRef HTMLInputElement
editId n = MkRef Input "edit_\{show n}"

refId : ElemRef t -> (Attribute ev)
refId ref = id ref.id

itemView : Item -> Node Ev
itemView (MkItem n lbl done) =
  li [ refId (liId n), class (if done then "completed" else "") ]
     [ div [ refId (viewId n), class "view" ]
           [ input [ class "toggle", type CheckBox, checked done 
                   , onChecked (Toggle n) ] []
           , label [ onDblClick (Edit n) ] [ Text lbl ]
           , button [ class "destroy", onClick (Del n) ] []
           ]
     , input [ refId (editId n), class "edit", value lbl 
             , onEnterDown (Upd n), onEscDown (Abort n), onBlur (Upd n) ] []
     ]

--------------------------------------------------------------------------------
--          Controller
--------------------------------------------------------------------------------

M : Type -> Type
M = DomIO Ev JSIO

-- attribute : (App -> HTMLElement) -> MSF MSt (Attribute Ev) ()
-- attribute f = fan [get >>^ f, id] >>> np setAttribute >>! lift

modItems : MonadState App m => (List Item -> List Item) -> m ()
modItems f = modify $ record { items $= f }

modItem : MonadState App m => Nat -> (Item -> Maybe Item) -> m ()
modItem n f = modItems $ mapMaybe (\i => if i.id == n then f i else Just i) 

newId : MonadState App m => m Nat
newId = modify (record { id $= S }) >> map id get

data Cmd = Add Item | Delete Nat | Update Item | Redraw | DoNothing

mod : Ev -> StateT App M ()
mod New       =   [| MkItem newId (get >>= getValue . newEl) (pure False) |]
              >>= \i => modItems (i ::)
mod Clear     = modItems (filter $ not . done)
mod (Abort k) = ?mod_rhs_3
mod (Mark b)  = modItems (map $ record { done = b })
mod (Edit k) = ?mod_rhs_5
mod (Del k) = modItem k (const Nothing)
mod (Upd k) = ?mod_rhs_7
mod (Toggle k b) = modItem k $ Just . record { done = b }

msf : MSF (StateT App M) Ev ()

ui : M (MSF M Ev (), JSIO ())
ui = do
  tdNew    <- getElementByClass "new-todo"
  tdMain   <- getElementByClass "main"
  tdList   <- getElementByClass "todo-list"
  tdFooter <- getElementByClass "footer"
  tdCount  <- getElementByClass "todo-count"
  tdClear  <- getElementByClass "clear-completed"
  tdAll    <- getElementByClass "toggle-all"

  setAttributes (up tdNew)   [onEnterDown New]
  setAttributes (up tdClear) [onClick Clear]
  setAttributes (up tdAll)   [onChecked Mark]

  let ini = MkApp 0 Nil tdNew tdMain tdList tdFooter tdCount tdClear tdAll

  pure (loopState ini msf, pure ())

main : IO ()
main = runJS . ignore $ reactimateDomIni Clear "todo" ui
