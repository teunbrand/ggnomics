# Currently only identity transformation
S4TransIdentity <- scales::trans_new(
    "S4identity",
    "force",
    "force",
    breaks = S4BreaksMajor,
    minor_breaks = S4BreaksMinor,
    format = S4LabelFormat
)
