pub struct DiagLocation {
    file: String,
    line: u32,
    col: u32,
}

pub enum DiagLevel {
    /// Compiler bug.
    Bug,
    /// User error, compilation cannot complete.
    Error,
    /// User mistake, compilation can complete but behavior may be unintentional.
    Warning,
}

pub struct DiagInner {
    pub level: DiagLevel,
    pub location: DiagLocation,
    // TODO: Error handling isn't great right now, just spit out a string.
    pub message: String,
}
