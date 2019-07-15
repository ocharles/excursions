{-# language BlockArguments #-}
{-# language EmptyCase #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language FunctionalDependencies #-}
{-# language GADTs #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language LambdaCase #-}
{-# language PolyKinds #-}
{-# language QuantifiedConstraints #-}
{-# language RankNTypes #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}
{-# language TypeOperators #-}
{-# language UndecidableInstances #-}

{-# options_ghc -Wall -fwarn-redundant-constraints #-}

module Reflected where

import Data.Tuple ( swap )
import Control.Monad.Trans.Except ( ExceptT(..), runExceptT )
import Control.Monad.Trans.State.Strict ( StateT(..) )
import Data.Coerce ( coerce )
import Data.Functor.Identity
import Unsafe.Coerce ( unsafeCoerce )


-- monad-trans-control, but functorially

class ( Functor f, forall m. Monad m => ( Monad ( t m ) ) ) => Transformer t f | t -> f where
  liftWithRun
    :: Monad m
    => ( ( forall x. t m x -> m ( f x ) ) -> m a )
    -> t m a

  restore :: Monad m => m ( f a ) -> t m a


instance Transformer ( ExceptT e ) ( Either e ) where
  liftWithRun f =
    ExceptT ( Right <$> f runExceptT )

  restore =
    ExceptT


instance Transformer ( StateT s ) ( (,) s ) where
  liftWithRun f =
    StateT ( \s -> (,) <$> f ( \( StateT g ) -> swap <$> g s ) <*> pure s )

  restore m =
    StateT ( \_ -> swap <$> m )



-- Programs

newtype Program sig carrier a =
  Program { runProgram :: carrier a }
  deriving
    ( Functor, Applicative, Monad )



-- Interpretation of an effect via a monad transformer.


newtype Interpreted sig carrier a =
  Interpreted
    { runInterpreted
        :: forall s. Reifies s ( Handler sig carrier ) => Program sig ( ScopedT s carrier ) a
    }


instance Functor carrier => Functor ( Interpreted sig carrier ) where
  {-# inline fmap #-}
  fmap f ( Interpreted a ) =
    Interpreted ( fmap f a )


instance Applicative carrier => Applicative ( Interpreted sig carrier ) where
  {-# inline pure #-}
  pure a =
    Interpreted ( pure a )

  {-# inline (<*>) #-}
  Interpreted f <*> Interpreted a =
    Interpreted ( f <*> a )


instance Monad carrier => Monad ( Interpreted sig carrier ) where
  {-# inline return #-}
  return a =
    Interpreted ( return a )

  {-# inline (>>=) #-}
  Interpreted a >>= f =
    Interpreted ( a >>= runInterpreted . f )


{-# inline interpretT #-}
interpretT
  :: forall e m g a t f.
     ( Transformer t f, AnEffect e, Carrier g m, Effect g )
  => ( forall x. t m x -> m ( f x ) )
  -> ( forall x. f ( t m x ) -> m ( f x ) )
  -> ( forall x. e ( t m ) x -> t m x )
  -> Interpreted ( e + g ) ( t m ) a
  -> m ( f a )
interpretT out distribute runOp p =
  reify ( Handler handler ) ( go p )

  where

    handler
      :: forall s x.
         ( e + g ) ( Program ( e + g ) ( ScopedT s ( t m ) ) ) x
      -> Program ( e + g ) ( ScopedT s ( t m ) ) x
    handler = \case
      L ( Coyoneda f a ) ->
        coerce $ fmap f $
        case handle2 ( Identity () ) ( fmap Identity . coerce ) a of
          Coyoneda g e ->
            runIdentity . g <$> runOp e

      R a -> Program $ ScopedT $ do
        ctx <-
          liftWithRun \f -> f ( return () )

        restore ( eff ( handle ctx ( distribute . fmap ( unScopedT . runProgram ) ) a ) )


    go
      :: forall x s.
         Reifies s ( Handler ( e + g ) ( t m ) )
      => Interpreted ( e + g ) ( t m ) x
      -> Tagged s ( m ( f x ) )
    go ( Interpreted p' ) =
      Tagged ( out ( coerce ( p' :: Program ( e + g ) ( ScopedT s ( t m ) ) x ) ) )


newtype ScopedT ( s :: * ) m a =
  ScopedT { unScopedT :: m a }
  deriving
    ( Functor, Applicative, Monad)



-- Coyoneda


data Coyoneda f a where
  Coyoneda :: ( a -> b ) -> f a -> Coyoneda f b


instance Functor ( Coyoneda f ) where
  fmap f ( Coyoneda g a ) =
    Coyoneda ( f . g ) a



-- Effect sums


data ( f + g ) m a =
  L ( Coyoneda ( f m ) a ) | R ( g m a )


class Member e sig where
  inj :: e m a -> sig m a


instance {-# overlaps #-} Member e ( e + f ) where
  inj =
    L . Coyoneda id


instance {-# overlappable #-} Member e g => Member e ( f + g ) where
  inj = R . inj



-- Reified handlers for signatures


newtype Handler sig m =
  Handler
    { runHandler
        :: forall s a. sig ( Program sig ( ScopedT s m ) ) a
        -> Program sig ( ScopedT s m ) a
    }



-- reflection


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



-- Carriers

class Monad m => Carrier sig m | m -> sig where
  eff :: sig m a -> m a


instance ( Monad m, Reifies s ( Handler sig m ) ) => Carrier sig ( Program sig ( ScopedT s m ) ) where
  {-# inline eff #-}
  eff =
    runHandler ( unTag ( reflect @s ) )


instance ( Monad m, Effect sig ) => Carrier sig ( Interpreted sig m ) where
  {-# inline eff #-}
  eff a =
    Interpreted ( go a )

    where

      go
        :: forall s x.
           Reifies s ( Handler sig m )
        => sig ( Interpreted sig m ) x
        -> Program sig ( ScopedT s m ) x
      go x =
        fmap runIdentity ( eff ( handle ( Identity () ) ( fmap Identity . runInterpreted . runIdentity ) x ) )



-- Effects


class Effect e where
  handle
    :: ( Functor f )
    => f ()
    -> ( forall x. f ( m x ) -> n ( f x ) )
    -> e m a
    -> e n ( f a )


class AnEffect e where
  handle2
    :: ( Functor f )
    => f ()
    -> ( forall x. f ( m x ) -> n ( f x ) )
    -> e m a
    -> Coyoneda ( e n ) ( f a )


instance ( AnEffect f, Effect g ) => Effect ( f + g ) where
  handle ctx eta = \case
    L ( Coyoneda f a ) ->
      L ( fmap ( fmap f ) ( handle2 ctx eta a ) )

    R a ->
      R ( handle ctx eta a )



-- PURE

data Pure m a


instance Effect Pure where
  handle _ _ = \case {}


instance Carrier Pure Identity where
  eff = \case {}


run :: Identity a -> a
run p =
  coerce p



-- STATE

data State s m a where
  Get :: State s m s
  Put :: s -> State s m ()


instance AnEffect ( State s ) where
  handle2 ctx _ = \case
    Get -> Coyoneda ( <$ ctx ) Get
    Put s -> Coyoneda ( <$ ctx ) ( Put s )


get :: ( Carrier sig m, Member ( State s ) sig ) => m s
get =
  eff ( inj Get )


put :: ( Carrier sig m, Member ( State s ) sig ) => s -> m ()
put s =
  eff ( inj ( Put s ) )


modify :: ( Carrier sig m, Member ( State s ) sig ) => ( s -> s ) -> m ()
modify f =
  get >>= put . f


{-# inline execState #-}
execState
  :: forall s m sig a.
     ( Carrier sig m, Effect sig )
  => s
  -> Interpreted ( State s + sig ) ( StateT s m ) a
  -> m s
execState s0 p =
  fmap fst $
  interpretT
    ( \m -> swap <$> runStateT m s0 )
    ( \( s, m ) -> swap <$> runStateT m s )
    ( \case
        Get ->
          StateT ( \s -> return ( s, s ) )

        Put s' ->
          StateT ( \_ -> return ( (), s' ) )
    )
    p


-- WRITER

data Writer w m a where
  Tell :: w -> Writer w m ()


instance AnEffect ( Writer w ) where
  handle2 ctx _ ( Tell w ) = Coyoneda ( <$ ctx ) ( Tell w )


tell :: ( Carrier sig m, Member ( Writer w ) sig ) => w -> m ()
tell =
  eff . inj . Tell


{-# inline execWriter #-}
execWriter
  :: forall w m sig a.
     ( Monoid w, Carrier sig m, Effect sig )
  => Interpreted ( Writer w + sig ) ( StateT w m ) a
  -> m w
execWriter p =
  fmap fst $
  interpretT
    ( \m -> swap <$> runStateT m mempty )
    ( \( w, s ) -> swap <$> runStateT s w )
    ( \case
        Tell w ->
          StateT ( \s -> return ( (), s <> w ) )
    )
    p

