{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Music where

import Test.Hspec
import Data.Ratio

type Pitch = (PitchClass, Octave)
data PitchClass = Cf | C | Cs | Df | D | Ds | Ef | E | Es | Ff | F | Fs | Gf | G | Gs | Af | A | As | Bf | B | Bs
  deriving (Eq, Show)

type Octave = Int

data Music
  = Note Pitch Dur
  | Rest Dur
  | Music :+: Music
  | Music :=: Music
  | Tempo (Ratio Int) Music
  | Trans Int Music
  | Instr IName Music

type Dur = Ratio Int

type AbsPitch = Int

data IName
 = AcousticGrandPiano  | BrightAcousticPiano | ElectricGrandPiano
 | HonkyTonkPiano      | RhodesPiano         | ChorusedPiano
 | Harpsichord   | Clavinet        | Celesta | Glockenspiel  | MusicBox
 | Vibraphone | Marimba  | Xylophone           | TubularBells
 | Dulcimer              | HammondOrgan        | PercussiveOrgan
 | RockOrgan | ChurchOrgan         | ReedOrgan
 | Accordion             | Harmonica           | TangoAccordion
 | AcousticGuitarNylon   | AcousticGuitarSteel | ElectricGuitarJazz
 | ElectricGuitarClean   | ElectricGuitarMuted | OverdrivenGuitar
 | DistortionGuitar      | GuitarHarmonics     | AcousticBass
 | ElectricBassFingered  | ElectricBassPicked  | FretlessBass
 | SlapBass1             | SlapBass2           | SynthBass1 | SynthBass2
 | Violin        | Viola | Cello  | Contrabass | TremoloStrings
 | PizzicatoStrings      | OrchestralHarp      | Timpani
 | StringEnsemble1       | StringEnsemble2     | SynthStrings1
 | SynthStrings2         | ChoirAahs           | VoiceOohs | SynthVoice
 | OrchestraHit          | Trumpet             | Trombone  | Tuba
 | MutedTrumpet          | FrenchHorn          | BrassSection | SynthBrass1
 | SynthBrass2           | SopranoSax          | AltoSax | TenorSax
 | BaritoneSax    | Oboe | Bassoon  | EnglishHorn          | Clarinet
 | Piccolo               | Flute    | Recorder | PanFlute  | BlownBottle
 | Shakuhachi            | Whistle  | Ocarina  | Lead1Square
 | Lead2Sawtooth         | Lead3Calliope       | Lead4Chiff
 | Lead5Charang          | Lead6Voice          | Lead7Fifths
 | Lead8BassLead         | Pad1NewAge          | Pad2Warm
 | Pad3Polysynth         | Pad4Choir           | Pad5Bowed
 | Pad6Metallic          | Pad7Halo            | Pad8Sweep
 | FX1Train              | FX2Soundtrack       | FX3Crystal
 | FX4Atmosphere         | FX5Brightness       | FX6Goblins
 | FX7Echoes             | FX8SciFi            | Sitar | Banjo  | Shamisen
 | Koto | Kalimba        | Bagpipe             | Fiddle | Shanai
 | TinkleBell    | Agogo | SteelDrums          | Woodblock      | TaikoDrum
 | MelodicDrum           | SynthDrum           | ReverseCymbal
 | GuitarFretNoise       | BreathNoise         | Seashore
 | BirdTweet             | TelephoneRing       | Helicopter
 | Applause              | Gunshot             | Percussion
 deriving (Show,Eq,Ord,Enum)

absPitch :: Pitch -> AbsPitch
absPitch (pc, oct) = 12 * oct + pcToInt pc

pitch :: AbsPitch -> Pitch
pitch ap = ([C, Cs, D, Ds, E, F, Fs, G, Gs, A, As, B] !! mod ap 12, quot ap 12)

pcToInt :: PitchClass -> Int
pcToInt pc = case pc of
               Cf -> -1
               C -> 0
               Cs -> 1
               Df -> 1
               D -> 2
               Ds -> 3
               Ef -> 3
               E -> 4
               Es -> 5
               Ff -> 4
               F -> 5
               Fs -> 6
               Gf -> 6
               G -> 7
               Gs -> 8
               Af -> 8
               A -> 9
               As -> 10
               Bf -> 10
               B -> 11
               Bs -> 12

trans :: Int -> Pitch -> Pitch
trans i p = (pitch (absPitch p + i))

cf, c, cs, df, d, ds, ef, e, es, ff, f, fs, gf, g, gs, af, a, as, bf, b, bs :: Octave -> Dur -> Music
cf o = Note (Cf, o)
c o = Note(C, o)
cs o = Note(Cs, o)
df o = Note(Df, o)
d o = Note(D, o)
ds o = Note(Ds, o)
ef o = Note(Ef, o)
e o = Note(E, o)
es o = Note(Es, o)
ff o = Note(Ff, o)
f o = Note(F, o)
fs o = Note(Fs, o)
gf o = Note(Gf, o)
g o = Note(G, o)
gs o = Note(Gs, o)
af o = Note(Af, o)
a o = Note(A, o)
as o = Note(As, o)
bf o = Note(Bf, o)
b o = Note(B, o)
bs o = Note(Bs, o)

wn, hn, qn, en, sn, tn :: Dur
dhn, dqn, den, dsn :: Dur

wnr, hnr, qnr, enr, snr, tnr :: Music
dhnr, dqnr, denr, dsnr :: Music

wn = 1; wnr = Rest wn
hn = 1%2; hnr = Rest hn
qn = 1%4; qnr = Rest qn
en = 1%8; enr = Rest en
sn = 1%16; snr = Rest sn
tn = 1%32; tnr = Rest tn

dhn = 3%4; dhnr = Rest dhn
dqn = 3%8; dqnr = Rest dqn
den = 3%16; denr = Rest den
dsn = 3%32; dsnr = Rest dsn

line, chord :: [Music] -> Music
line = foldr (:+:) (Rest 0)
chord = foldr (:=:) (Rest 0)

cMaj :: [Music]
cMaj = [n 4 qn | n <- [c, e, g]]


cMajArp, cMajChd :: Music
cMajArp = line cMaj
cMajChd = chord cMaj

delay :: Dur -> Music -> Music
delay del m = Rest del :+: m

repeatM :: Music -> Music
repeatM m = m :+: repeatM m

dur :: Music -> Dur
dur (Note _ du) = du
dur (Rest du) = du
dur (m1 :+: m2) = dur m1 + dur m2
dur (m1 :=: m2) = dur m1 `max` dur m2
dur (Tempo t m) = dur m / t
dur (Trans _ m) = dur m
dur (Instr _ m) = dur m

revM :: Music -> Music
revM n@(Note _ _) = n
revM r@(Rest _) = r
revM (Tempo t m) = Tempo t (revM m)
revM (Trans dt m) = Trans dt (revM m)
revM (Instr i m) = Instr i (revM m)
revM (m1 :+: m2) = m2 :+: m1
revM (m1 :=: m2) = let d1 = dur m1
                       d2 = dur m2
                   in case () of
                        _ | d1 < d2 -> (Rest (d2 - d1) :+: revM m1) :=: revM m2
                        _ | d1 > d2 -> revM m1 :=: (Rest (d1 - d2) :+: revM m2)
                        _ -> revM m1 :=: revM m2

cut :: Dur -> Music -> Music
cut dr _ | dr <= 0 = Rest 0
cut dr (Note x d0) = Note x (min d0 dr)
cut dr (Rest d0) = Rest (min d0 dr)
cut dr (Tempo t m) = Tempo t (cut (dr * t) m)
cut dr (Trans dt m) = Trans dt (cut dr m)
cut dr (Instr i m) = Instr i (cut dr m)
cut dr (m1 :=: m2) = cut dr m1 :=: cut dr m2
cut dr (m1 :+: m2) = let m1' = cut dr m1
                    in m1' :+: cut (dr - dur m1') m2

(/=/) :: Music -> Music -> Music
m1 /=/ m2 = cut (minDur m1 m2) (m1 :=: m2)

minDur :: Music -> Music -> Dur
minDur m1 m2 =
  let
    stamps1 = fst $ musicEndTimestamps 0 1 m1
    stamps2 = fst $ musicEndTimestamps 0 1 m2
    go cur [] _ = cur
    go cur _ [] = cur
    go _ as@(a:as') bs@(b:bs')
      | a < b = go a as' bs
      | otherwise = go b as bs'

  in go 0 stamps1 stamps2

musicEndTimestamps :: Dur -> Ratio Int -> Music -> ([Dur], Dur)
musicEndTimestamps t0 tempo (Note _ dr) =
  let
    t1 = t0 + dr * tempo
  in ([t1], t1)
musicEndTimestamps t0 tempo (Rest dr) =
  let
    t1 = t0 + dr * tempo
  in ([t1], t1)
musicEndTimestamps t0 tempo (Trans _ m) = musicEndTimestamps t0 tempo m
musicEndTimestamps t0 tempo (Instr _ m) = musicEndTimestamps t0 tempo m
musicEndTimestamps t0 tempo (Tempo t m) = musicEndTimestamps t0 (tempo / t) m
musicEndTimestamps t0 tempo (m1 :+: m2) =
  let
    (stamps1, last1) = musicEndTimestamps t0 tempo m1
    (stamps2, last2) = musicEndTimestamps last1 tempo m2
  in (stamps1 ++ stamps2, last2)
musicEndTimestamps t0 tempo (m1 :=: m2) =
  let
    (stamps1, last1) = musicEndTimestamps t0 tempo m1
    (stamps2, last2) = musicEndTimestamps t0 tempo m2
  in
    (joinTimestamps stamps1 stamps2, max last1 last2)

joinTimestamps :: [Ratio Int] -> [Ratio Int] -> [Ratio Int]
joinTimestamps [] s2 = s2
joinTimestamps s1 [] = s1
joinTimestamps as@(a:as') bs@(b:bs')
  | a < b = a : joinTimestamps as' bs
  | otherwise = b : joinTimestamps as bs'

shortestParallelSpec :: Spec
shortestParallelSpec = describe "/=/" $ do
  let mu1 = c 4 10
      mu2 = c 4 5
  describe "handles simple cases" $ do
    it "with one order" $ do
      dur (mu1 /=/ mu2) `shouldBe` 5
    it "with reverse order" $ do
      dur (mu2 /=/ mu1) `shouldBe` 5
  describe "handles one infinite music" $ do
    it "right one" $ do
      dur (mu1 /=/ repeatM mu2) `shouldBe` 10
    it "left one" $ do
      dur (repeatM mu1 /=/ mu2) `shouldBe` 5
  describe "handles more complex cases" $ do
    it "with more parallelism" $ do
      dur ((Rest 5 :=: repeatM mu2) /=/ mu1) `shouldBe` 10
  describe "handles tempo change" $ do
    it "for slowing down" $ do
      dur (Tempo 3 mu1 /=/ mu2) `shouldBe` (10%3)

testMinDur :: IO ()
testMinDur = hspec $ do
  shortestParallelSpec

trill :: Int -> Dur -> Music -> Music
trill i d n@(Note p nd) =
  if d >= nd then n
  else Note p d :+: trill (negate i) d (Note (trans i p) (nd - d))
trill i d (Tempo a m) = Tempo a (trill i (d * a) m)
trill i d (Trans a m) = Trans a (trill i d m)
trill i d (Instr a m) = Instr a (trill i d m)
trill _ _ _ = error "Trill input must be a single note"

trill' :: Int -> Dur -> Music -> Music
trill' i sDur m = trill (negate i) sDur (Trans i m)

roll :: Dur -> Music -> Music
roll dur m = trill 0 dur m
