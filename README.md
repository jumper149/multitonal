# multitonal

This is a Haskell library for composing and analysing harmony.

You can calculate the offset of intervals in different tuning systems (here with equal temperament and the intervals from C to each note in the C Lydian scale:
    λ *Multitonal > putStrLn . pp $ (id Control.Arrow.&&& correctCent standardTuning (C :- 4)) <$> ((:- 4) <$> listNotes (scale Lydian C))
    C₄, 0ct
    D₄, 4ct
    E₄, -14ct
    F♯₄, -10ct
    G₄, 2ct
    A₄, -16ct
    B₄, -12ct

You can also view the chords regarding different Modes and Functions of chords:
    λ *Multitonal > putStrLn . pp $ seventh Lydian <$> [minBound..maxBound] <*> pure C
    Chord C E G B
    Chord D F♯ A C
    Chord E G B D
    Chord F♯ A C E
    Chord G B D F♯
    Chord A C E G
    Chord B D F♯ A

And if you think that is a little bit unreadable you can give common names to the chords:
    λ *Multitonal > putStrLn . pp $ fmap nameChord $ seventh Lydian <$> [minBound..maxBound] <*> pure C
    Cmaj⁷
    Ddom⁷
    Emin⁷
    F♯min⁷♭5
    Gmaj⁷
    Amin⁷
    Bmin⁷
