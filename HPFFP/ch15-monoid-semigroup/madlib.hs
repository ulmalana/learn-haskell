import Data.Monoid

type Verb = String
type Adjective = String
type Adverb = String
type Noun = String
type Exclamation = String

madlibbing :: Exclamation
            -> Adverb
            -> Noun
            -> Adjective
            -> String
madlibbing e adv noun adj =
    e <> "! he said " <>
    adv <> " as he jumped into his car " <>
    noun <> " and drove off with his " <>
    adj <> " wife"

-- madlibbing with mconcat
madlibbingbetter :: Exclamation
                 -> Adverb
                 -> Noun
                 -> Adjective
                 -> String
madlibbingbetter e adv noun adj = 
    mconcat [e, "! he said ", adv, " as he jumped into his car ", noun, " and drove off with his ", adj, " wife"]
