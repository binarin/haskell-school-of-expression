module Music where

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

minDur :: Music -> Music -> Dur
minDur m1 m2 = undefined


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

(/=:) :: Music -> Music -> Music
m1 /=: m2 = cut (min (dur m1) (dur m2)) (m1 :=: m2)
