module FAL where

import Region
import SOE hiding (Region, Event)
import qualified SOE as G (Region, Event, Point)
import Animation (picToGraphic)
import Shape
import Picture
import Control.Arrow
import Memo1
import Draw (pixelToInch, xWin2, yWin2)

import Data.Functor (($>))

infixr 1 =>>, ->>
infixr 1 `untilB`, `switch`
-- infixr 1 `untilB`, `switch`, `stepAccum`, `step`
infixl 0 .|.
-- infixr 4 <*, >*
-- infixr 3 &&*
-- infixr 2 ||*

type Time = Float

type UserAction = G.Event

newtype Behavior a = Behavior (([Maybe UserAction], [Time]) -> [a])

newtype Event a = Event (([Maybe UserAction], [Time]) -> [Maybe a])

time :: Behavior Time
time = Behavior (\(_, ts) -> ts)

-- instance Semigroup a => Semigroup (Behavior a) where
--   (Behavior b1) <> (Behavior b2) = Behavior $ \args -> zipWith (<>) (b1 args) (b2 args)

-- instance Monoid a => Monoid (Behavior a) where
--   mempty = Behavior $ \_ -> mempty

instance Functor Behavior where
  fmap f (Behavior b) = Behavior $ \t -> f `fmap` b t

instance Applicative Behavior where
  pure = Behavior . const . repeat
  Behavior f <*> Behavior b = Behavior $ \t -> zipWith ($) (f t) (b t)

instance Functor Event where
  fmap f (Event b) = Event $ \uts -> (fmap f) <$> b uts


red, blue, white, yellow :: Behavior Color
red = pure Red
blue = pure Blue
white = pure White
yellow = pure Yellow

lift1 :: (a -> b) -> (Behavior a -> Behavior b)
lift1 = fmap

lift2 :: (a -> b -> c) -> (Behavior a -> Behavior b -> Behavior c)
lift2 f a b = f <$> a <*> b

lift3 :: (a -> b -> c -> d) -> (Behavior a -> Behavior b -> Behavior c -> Behavior d)
lift3 f a b c = f <$> a <*> b <*> c

untilB, switch :: Behavior a -> Event (Behavior a) -> Behavior a
Behavior bf `untilB` Event ef = memoB $ Behavior $ \uts@(us, ts) -> loop us ts (ef uts) (bf uts)
  where
    loop (_:us) (_:ts) ~(e:es) (b:bs) = b : case e of
      Nothing -> loop us ts es bs
      Just (Behavior bf') -> bf' (us, ts)

Behavior fb `switch` Event fe = memoB $ Behavior $ \uts@(us, ts) -> loop us ts (fe uts) (fb uts)
  where
    loop (_:us) (_:ts) ~(e:es) (b:bs) = b : case e of
      Nothing -> loop us ts es bs
      Just (Behavior fb') -> loop us ts es (fb' (us, ts))


memoB :: Behavior a -> Behavior a
memoB (Behavior fb) = Behavior (memo1 fb)

lbp :: Event ()
lbp = Event $ \(uas, _) -> getlbp <$> uas
  where
    getlbp (Just (Button _ True True)) = Just ()
    getlbp _ = Nothing


(->>) :: Event () -> Behavior Color -> Event (Behavior Color)
(->>) = ($>)

(=>>) :: Event a -> (a -> b) -> Event b
(=>>) = flip fmap

while :: Behavior Bool -> Event ()
while (Behavior fb) = Event $ \uts -> aux <$> fb uts
  where
    aux True = Just ()
    aux False = Nothing

unique :: Eq a => Event a -> Event a
unique (Event fe) = Event $ \uts -> aux (fe uts)
  where
    aux xs = zipWith remdup (Nothing : xs) xs
    remdup x y | x == y = Nothing
               | otherwise = y

when :: Behavior Bool -> Event ()
when = unique . while

integral :: Behavior Float -> Behavior Float
integral (Behavior fb) = Behavior $ \uts@(us, t:ts) -> 0 : loop t 0 ts (fb uts)
  where
    loop t1 acc (t2:ts) (a:as) =
      let acc' = acc + (t2 - t1)*a
      in acc' : loop t2 acc' ts as


withElem :: Event a -> [b] -> Event (a, b)
withElem (Event fe) bs = Event $ \uts -> loop (fe uts) bs
  where
    loop (Nothing:es) bs = Nothing : loop es bs
    loop (Just a:es) (b:bs) = Just (a,b) : loop es bs

withElem_ :: Event a -> [b] -> Event b
withElem_ e bs = snd <$> e `withElem` bs

(.|.) :: Event a -> Event a -> Event a
Event fl .|. Event fr = Event $ \uts -> zipWith aux (fl uts) (fr uts)
  where
    aux Nothing Nothing = Nothing
    aux l@(Just _) _ = l
    aux _ r@(Just _) = r

key :: Event Char
key = Event $ \(uas, _) -> map getkey uas
  where
    getkey (Just (Key ch True)) = Just ch
    getkey _ = Nothing

snapshot :: Event a -> Behavior b -> Event (a, b)
Event fe `snapshot` Behavior fb = Event $ \uts -> zipWith aux (fe uts) (fb uts)
  where
    aux Nothing _ = Nothing
    aux (Just a) b = Just (a, b)

snapshot_ :: Event a -> Behavior b -> Event b
snapshot_ e b = e `snapshot` b =>> snd

step :: a -> Event a -> Behavior a
step a (Event fe) = Behavior $ \uts -> a : loop a (fe uts)
  where
    loop a (Nothing:es) = a : loop a es
    loop a (Just a':es) = a' : loop a' es

stepAccum :: a -> Event (a -> a) -> Behavior a
-- stepAccum init (Event ff) = Behavior $ \uts -> init : loop init (ff uts)
--   where
--     loop val (Nothing:es) = val : loop val es
--     loop val (Just f:es) = let val' = f val
--                            in val' : loop val' es
a `stepAccum` e = b
  where
    b = a `step` (e `snapshot` b =>> uncurry ($))

mm :: Event Coordinate
mm = Event $ \(uas, _) -> getmm <$> uas
  where
    getmm (Just (MouseMove pt)) = Just (gPtToPt pt)
    getmm _ = Nothing

gPtToPt :: G.Point -> Coordinate
gPtToPt (x, y) = (pixelToInch (x - xWin2)
                 ,pixelToInch (yWin2 - y)
                 )

mouse :: (Behavior Float, Behavior Float)
mouse = (fstB m, sndB m)
  where m = (0, 0) `step` mm

fstB :: Behavior (a, b) -> Behavior a
fstB = fmap fst

sndB :: Behavior (a, b) -> Behavior b
sndB = fmap snd

translate :: (Behavior Float, Behavior Float) -> Behavior Region -> Behavior Region
translate (dx, dy) reg = tr <*> reg
  where
    vb :: Behavior Vector
    vb = (,) <$> dx <*> dy

    tr :: Behavior (Region -> Region)
    tr = Translate <$> vb
