{-# language EmptyCase #-}
{-# language UndecidableInstances #-}
{-# language QuantifiedConstraints #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language FunctionalDependencies #-}
{-# language GADTs #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language LambdaCase #-}
{-# language PolyKinds #-}
{-# language RankNTypes #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}

module Reflected where

import Control.Monad.Trans.Identity
import Data.Functor.Identity
import Data.Coerce ( coerce )
import Control.Monad ( ap, liftM )
import Control.Monad.Trans.Class ( MonadTrans, lift )
import Control.Monad.Trans.Except ( ExceptT(..), runExceptT )
import Unsafe.Coerce ( unsafeCoerce )


newtype Program sig carrier a =
  Program { runProgram :: carrier a }
  deriving
    ( Functor, Applicative, Monad )


newtype ScopedT ( s :: * ) m a =
  ScopedT { unScopedT :: m a }
  deriving
    ( Functor, Applicative, Monad)


data Sum f g m a =
  L ( f m a ) | R ( g m a )


class Member e sig where
  inj :: e m a -> sig m a


instance {-# overlaps #-} Member e ( Sum e f ) where
  inj = L


instance {-# overlappable #-} Member e g => Member e ( Sum f g ) where
  inj = R . inj


newtype Handler sig m =
  Handler { runHandler :: forall s a. sig ( Program sig ( ScopedT s m ) ) a -> Program sig ( ScopedT s m ) a }


newtype Tagged a b =
  Tagged { unTag :: b }


class Reifies ( s :: * ) a | s -> a where
  reflect :: Tagged s a


data Skolem


newtype Magic a r =
  Magic ( Reifies Skolem a => Tagged Skolem r )


reify :: forall a r. a -> ( forall s. Reifies s a => Tagged s r ) -> r
reify a k =
  unsafeCoerce ( Magic @a k ) a


data Error e m a where
  Throw :: e -> Error e m a
  TryCatch :: m a -> ( e -> m a ) -> Error e m a


class Monad m => Carrier sig m | m -> sig where
  eff :: sig m a -> m a


instance ( Effect sig, Carrier sig m, Monad m ) => Carrier sig ( ScopedT s m ) where
  eff a =
    fmap runIdentity ( ScopedT ( eff ( handle ( Identity () ) ( fmap Identity . unScopedT . runIdentity ) a ) ) )


instance ( Monad m, Reifies s ( Handler sig m ) ) => Carrier sig ( Program sig ( ScopedT s m ) ) where
  eff =
    runHandler ( unTag ( reflect @s ) )


class Effect e where
  handle
    :: ( Functor f )
    => f ()
    -> ( forall x. f ( m x ) -> n ( f x ) )
    -> e m a
    -> e n ( f a )


runError
  :: forall s' m g a e.
     ( Effect g, Carrier g m )
  => (    forall s.
          Reifies s ( Handler ( Sum ( Error e ) g ) ( ExceptT e m ) )
       => Program ( Sum ( Error e ) g ) ( ScopedT s ( ExceptT e m ) ) a
     )
  -> Program g m ( Either e a )
runError p =
  reify ( Handler handler ) ( go p )

  where

    handler
      :: forall s a.
         Sum ( Error e ) g ( Program ( Sum ( Error e ) g ) ( ScopedT s ( ExceptT e m ) ) ) a
      -> Program ( Sum ( Error e ) g ) ( ScopedT s ( ExceptT e m ) ) a
    handler = \case
      L a ->
        case a of
          Throw e ->
            coerce ( return @m ( Left e :: Either e a ) )

          TryCatch m f ->
            coerce $
              coerce m >>= \case
                Left e -> coerce ( f e )
                Right ok -> return @m ( Right ok :: Either e a )

      R a ->
        coerce $ eff @_ @m
          ( handle
              ( Right () :: Either e () )
              ( either ( return . Left ) coerce )
              a
          )


    go
      :: Program ( Sum ( Error e ) g ) ( ScopedT s ( ExceptT e m ) ) a
      -> Tagged s ( Program g m ( Either e a ) )
    go p' =
      coerce p'


throw :: ( Carrier sig m, Member ( Error e ) sig ) => e -> m a
throw e =
  eff ( inj ( Throw e ) )


program :: ( Carrier sig m, Member ( Error Bool ) sig ) => m Bool
program = do
  throw False
  return True


data Pure m a


instance Effect Pure where
  handle _ = \case {}


instance Monad m => Carrier Pure ( IdentityT m ) where
  eff = \case {}


runPure :: Program Pure ( IdentityT m ) a -> m a
runPure p =
  coerce p


test
  :: Monad m
  => m ( Either Bool Bool )
test =
  runPure ( runError program )
