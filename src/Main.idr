import Control.Monad.State
import Data.List
import Data.String
import Data.MSF.Trans
import Generics.Derive
import Rhone.JS

%default total
%language ElabReflection

record Item where
  constructor MkI
  id   : Nat
  todo : String
  done : Bool

data Ev = New | Clear | Mark Bool | Hash
        | Edit Nat | Abort Nat | Delete Nat | Upd Nat | Toggle Nat Bool

%runElab derive "Ev" [Generic,Eq]

new : ElemRef HTMLInputElement
new = Id Input "new-todo"

liId : Nat -> ElemRef HTMLLIElement
liId n = Id Li "item_\{show n}"

editId : Nat -> ElemRef HTMLInputElement
editId n = Id Input "edit_\{show n}"

itemView : Item -> Node Ev
itemView (MkI n lbl done) =
  li [ ref (liId n), class (if done then "completed" else "") ]
     [ div [ class "view" ]
           [ input [ class "toggle", type CheckBox, checked done 
                   , onChecked (Toggle n) ] []
           , label [ onDblClick (Edit n) ] [ Text lbl ]
           , button [ class "destroy", onClick (Delete n) ] [] ]
     , input [ ref (editId n), class "edit", value lbl 
             , onEnterDown (Upd n), onEscDown (Abort n), onBlur (Upd n) ] [] ]

ST : Type
ST = List Item

newId : List Item -> Nat
newId = maybe 0 (S . id) . getAt 0

mod : MonadState ST m => (i -> ST -> ST) -> MSF m i ()
mod f = arr f >>! modify

modAt : MonadState ST m => (i -> Item -> Item) -> MSF m (NP I [Nat,i]) ()
modAt f = mod $ \[n,v] => map (\t => if t.id == n then f v t else t)

newVal : LiftJSIO m => MSF m i (Event String)
newVal = valueOf new >>> runEffect (setValue new "") >>> trim ^>> isNot ""

countStr : Nat -> String
countStr 1 = "<strong>1</strong> item left"
countStr k = "<strong>\{show k}</strong> items left"

items : NP I [List Item, String] -> List (Node Ev)
items [is,"#/active"]    = map itemView . filter (not . done) $ reverse is
items [is,"#/completed"] = map itemView . filter done $ reverse is
items [is,_]             = map itemView $ reverse is

selected : LiftJSIO m => (String -> Bool) -> (id : String) -> MSF m String ()
selected p id = ifTrue p (const "selected") >>> attributeAt_ "class" (Id A id)

disp : MSF (StateT ST $ DomIO Ev JSIO) i ()
disp = get >>> fan
  [ fan [id, windowHash] >>> arr items >>! innerHtmlAtN (Id Ul "todo-list")
  , isNil ^>- [hiddenAt (Id Section "main"), hiddenAt (Id Footer "footer")]
  , all done ^>> isChecked (Id Input "toggle-all")
  , count (not . done) ^>> countStr ^>> innerHtml (Id Span "todo-count") ]
  >>> windowHash >>-
        [ selected (== "#/active") "sel-active"
        , selected (== "#/completed") "sel-completed"
        , selected (\s => s /= "#/completed" && s /= "#/active") "sel-all" ]

upd : MSF (StateT ST $ DomIO Ev JSIO) (NP I [Nat,String]) ()
upd = bool (\[_,s] => null s) >>> collect
        [ mod (\[i,_] => filter $ (/= i) . id)
        , modAt (\s => record {todo = s}) ]

msf : MSF (StateT ST $ DomIO Ev JSIO) Ev ()
msf = (toI . unSOP . from) ^>> collect
  [ newVal ?>> [| MkI (get >>^ newId) id (pure False) |] >>> mod (::) >>> disp
  , mod (\_ => filter $ not . done) >>> disp
  , mod (\[b] => map $ record {done = b}) >>> disp
  , disp
  , fan [ hd >>^ liId, const "editing" ] >>> attribute_ "class"
  , disp
  , mod (\[i] => filter $ (/= i) . id) >>> disp
  , hd >>> fan [id, editId ^>> value >>^ trim] >>> upd >>> disp
  , modAt (\b => record {done = b}) >>> disp ]

ui : DomIO Ev JSIO (MSF (DomIO Ev JSIO) Ev (), JSIO ())
ui =  setAttribute new (onEnterDown New)
   >> setAttribute (Id Button "clear-completed") (onClick Clear)
   >> setAttribute (Id Input "toggle-all") (onChecked Mark)
   >> handleEvent Window (HashChange Hash)
   $> (loopState Nil msf, pure ())

main : IO ()
main = runJS . ignore $ reactimateDomIni Clear "todo" ui
