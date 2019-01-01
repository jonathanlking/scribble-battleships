module Scribble.Concur where

import Scribble.FSM (class Initial, class ProtocolName, class ProtocolRoleNames, class RoleName, class Terminal, Protocol, Role)
import Effect.Aff (Aff, attempt)
import Control.Monad.Error.Class (throwError)
import Control.Coroutine (Consumer)
import Data.Tuple (Tuple(..))
import Prelude --(class Show, Unit, bind, discard, pure, show, unit, ($), (<$>), (<*>), (<>), (>>=))
import Type.Row (class ListToRow, kind RowList, RLProxy(RLProxy))
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Control.Monad.Trans.Class (lift)
import Data.Argonaut.Core (fromString)
import Scribble.Type.SList as SList
import Data.List ((:))
import Type.Proxy (Proxy)
import Scribble.Core (class Transport, encodeReq, uReceive, uSend, uOpen, uClose)
import Control.Monad.Free.Trans (hoistFreeT)
import Data.Either (Either(..))
import Effect.Exception (Error)
import Concur.Core (Widget)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Aff.AVar (AVar, new, empty, put, read, take)
-- import Scribble.Indexed (Session(..), Channel(..), open, close)

import Scribble.FSM (class Branch, class Initial, class ProtocolName, class ProtocolRoleNames, class Receive, class RoleName, class Select, class Send, class Terminal, Protocol, Role(..))
import Effect.Aff (Aff, delay)
import Effect.Aff.AVar (AVar, new, empty, put, read, take)
import Data.Time.Duration (Milliseconds(..))
import Control.Monad.Error.Class (throwError)
import Effect.Exception (error)
import Control.Coroutine as CR
import Data.Tuple (Tuple(..))
import Prelude (class Show, Unit, bind, discard, pure, show, unit, ($), (<$>), (<*>), (<>), (>>=), map)
import Control.Apply ((*>))
import Type.Row (class ListToRow, Cons, Nil, kind RowList, RLProxy(RLProxy))
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Record.Unsafe (unsafeGet, unsafeHas)
import Control.Monad.Trans.Class (lift)
import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Argonaut.Core (Json, fromArray, fromObject, fromString, toObject, toString)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), maybe)
import Effect.Class (liftEffect)
import Scribble.Type.SList as SList
import Data.List (List, (:))
import Data.Monoid (mempty)
import Foreign.Object (fromFoldable, lookup)
import Data.Array as Array
import Type.Proxy (Proxy)
import Data.String (toLower)
import Data.List.Types (List(..))
import Effect.Class.Console (log)
import Prim.TypeError

import Effect.Class
import Data.Functor
import Control.Apply
import Data.Tuple (snd)
import Control.Bind
import Control.Monad
import Control.Applicative
import Unsafe.Coerce (unsafeCoerce)
import Scribble.Core (class Transport, uOpen, uClose, uSend, uReceive)





-- From purescript-indexed-monad
class IxMonad m where
  ipure ∷ ∀ a x. a → m x x a
  ibind ∷ ∀ a b x y z. m x y a → (a → m y z b) → m x z b

newtype Session m c i t a = Session ((Channel c i) -> m (Tuple (Channel c t) a))

instance sessionIxMonad :: Monad m => IxMonad (Session m c) where
  ipure x = Session (\c -> pure (Tuple c x))
  ibind (Session s) f
    = Session (\c -> (s c) 
        >>= (\(Tuple c' x) -> case f x of
          (Session s') -> s' c'))

-- TODO: Work out how to derive these from the bind/pure definitions?
instance sessionFunctor :: Functor m => Functor (Session m c i i) where
  map f (Session s) = Session (\c -> map (\(Tuple _ x) -> Tuple c $ f x) $ s c)

instance sessionApply :: Apply m => Apply (Session m c i i) where
  apply (Session f) (Session s) = Session (\c -> apply (h $ f c) (s c))
    where
    h :: forall f a b c.
         Functor f
      => f (Tuple c (a -> b))
      -> f (Tuple c a -> Tuple c b)
    h = map (\(Tuple _ f) -> \(Tuple c x)  -> Tuple c (f x))

instance sessionBind :: Bind m => Bind (Session m c i i) where
  bind (Session s) f = Session (\c -> (s c) >>= (\(Tuple _ x) -> case f x of (Session s') -> s' c))

instance sessionApplicative :: Applicative m => Applicative (Session m c i i) where
  pure x = Session (\c -> pure (Tuple c x))

instance sessionMonad :: Monad m => Monad (Session m c i i)

instance sessionMonadEffect :: MonadEffect m => MonadEffect (Session m c i i) where
  liftEffect eff = Session (\c -> map (Tuple c) (liftEffect eff))

-- TODO: Find out why the `Monoid v` constraint isn't implied
instance sessionMonadAff :: MonadAff m => MonadAff (Session m c i i) where
  liftAff aff = Session (\c -> map (Tuple c) (liftAff aff))

liftWidget :: forall v c i a. Widget v a -> Session (Widget v) c i i a
liftWidget w = Session \c -> map (Tuple c) w

data Channel c s = Channel c (AVar (List Json))

-- | Open a new chanel and receive the initial state
open :: forall r n c s p.
     Initial r s
  => Transport c p 
  => RoleName r n
  => IsSymbol n
  => Role r -> p -> Aff (Channel c s)
open _ p = do
  bstack <- new Nil
  c <- uOpen p
  pure $ Channel c bstack

-- | We don't need to check linearity here, as in order to construct a terminal
-- | state, the previous state must have been consumed.
close :: forall r c s p.
     Terminal r s
  => Transport c p
  => Role r -> Channel c s -> Aff Unit
close _ (Channel c _) = uClose c

-- | Designed for a binary session (with direct communication)
session :: forall r n c p s t m a.
     Transport c p
  => Initial r s
  => Terminal r t
  => RoleName r n
  => IsSymbol n
  => MonadAff m
  => Proxy c
  -> Role r
  -> p
  -> Session m c s t a
  -> m a
session _ r p (Session prog) = do
  (c :: Channel c s) <- liftAff $ open r p
  (Tuple c' x) <- prog c
  liftAff $ close r c'
  pure x

send :: forall r rn c a s t m p. 
     Send r s t a
  => RoleName r rn
  => IsSymbol rn
  => Transport c p
  => EncodeJson a
  => MonadAff m
  => a -> Session m c s t Unit
send x = Session \c@(Channel t _) -> 
  map (Tuple (unsafeCoerce c)) 
    (liftAff $ uSend t $ encodeMessage (Role :: Role r) (encodeJson x))

receive :: forall r c a s t m p. 
     Receive r s t a
  => Transport c p
  => DecodeJson a
  => MonadAff m
  => Session m c s t a
receive = Session \c@(Channel t bv) ->
  map (Tuple (unsafeCoerce c)) $ liftAff $ do
    b <- take bv
    x <- case b of
      Nil -> do
        put Nil bv
        uReceive t
      (Cons v vs) -> (put vs bv) *> pure v
    case decodeJson x of
      Left e  -> throwError $ error e
      Right a -> pure a

-- | Label used for banching/selecting
newtype Label = Label String

instance encodeJsonLabel :: EncodeJson Label where
  encodeJson (Label l) = encodeJson l
instance decodeJsonLabel :: DecodeJson Label where
  decodeJson l = Label <$> decodeJson l

-- | `Functions` maps the 'dictionary' `ts` to a dictionary of continuations
-- | to common state `u` running in monad `m`
class Functions (im :: Type -> Type -> Type -> Type) (ts :: RowList) u (funcs :: RowList) | im ts u -> funcs
instance functionNil  :: Functions im Nil u Nil
instance functionCons :: Functions im tail u tail'
  => Functions im (Cons label t tail) u (Cons label (im t u Unit) tail')

-- | Constraint to assert element membership in RowList
class Elem (list :: RowList) (l :: Symbol) e | list l -> e
instance elemHead :: Elem (Cons l e tail) l e else
instance elemTail :: Elem tail l e => Elem (Cons l' e' tail) l e else
instance elemEmpty :: 
     Fail (Beside (Text l) (Text "is not a supported choice"))
  => Elem Nil l e 

choice :: forall r c s ts u funcs row m p.
     Branch r s ts
  => Terminal r u
  => Transport c p 
  => Functions (Session m c) ts u funcs
  => ListToRow funcs row
  => MonadAff m
  => Record row -> Session m c s u Unit
choice row = Session \c@(Channel ch bv) ->
  map (Tuple (unsafeCoerce c)) $ liftAff $ do
   x <- uReceive ch
   let lab = (toObject x >>= lookup "tag" >>= toString)
   let lab' = toLower <$> lab
   case lab' of
     Nothing -> throwError $ error "Unable to parse tag of message in branch"
     (Just label) -> if (unsafeHas label row)
                       then do
                          take bv >>= \vs -> put (Cons x vs) bv
                          (unsafeGet label row) c
                       else throwError (error $ "Branch chosen `"
                                              <> label  <> "`  is not supported")

select :: forall r rn c s ts t label m p.
     Select r s ts
  => RoleName r rn
  => IsSymbol rn
  => Transport c p
  => Elem ts label t
  => IsSymbol label
  => MonadAff m
  => SProxy label -> Session m c s t Unit
select _ = unsafeCoerce (pure unit :: Session m c s s Unit)

-- | Encode a message with role information for the proxy
encodeMessage :: forall r rn.
     RoleName r rn
  => IsSymbol rn
  => Role r
  -> Json
  -> Json
encodeMessage _ m = m
--encodeMessage _ m = fromObject $ fromFoldable $ (Tuple "to" $ fromString
--    (reflectSymbol (SProxy :: SProxy rn))) : (Tuple "body" m) : mempty 

newtype Identifier = Identifier String
instance identifierShow :: Show Identifier where
  show (Identifier i) = i

type SessionReq = { protocol :: Tuple String (List String), assignment :: List (Tuple String String), role :: String }

-- TODO: Rewrite this using pureST
encodeReq :: SessionReq -> Json
encodeReq req = fromObject $ fromFoldable $ (Tuple "protocol" protocol) : (Tuple "role" (fromString req.role)) : (Tuple "assignment" ass) : mempty
  where
    (Tuple name roles) = req.protocol
    protocol = fromObject $ fromFoldable $ (Tuple "name" (fromString name)) : (Tuple "roles" roles') : mempty
    roles' = fromArray $ fromString <$> Array.fromFoldable roles
    ass = fromObject $ fromString <$> fromFoldable req.assignment
