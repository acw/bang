use crate::syntax::ast::*;
use crate::syntax::error::ParserError;
use crate::syntax::parse::Parser;
use crate::syntax::tokens::Lexer;
use memmap2::Mmap;
use std::collections::HashMap;
use std::path::{Path, PathBuf};

#[derive(Default)]
pub struct Universe {
    pub files: HashMap<PathBuf, Mmap>,
    pub modules: HashMap<PathBuf, Module>,
}

impl Universe {
    /// Add a file to this universe.
    ///
    /// This may result in other files being loaded on behalf of the file, if
    /// (for example) the given file has imports.
    pub fn add_file<P: AsRef<Path>>(&mut self, file: P) -> Result<(), ParserError> {
        let filename = file.as_ref().to_string_lossy().into_owned();

        let file_handle = std::fs::File::open(&file).map_err(|e| ParserError::OpenError {
            file: filename.clone(),
            error: e,
        })?;
        let contents = unsafe { Mmap::map(&file_handle) }.map_err(|e| ParserError::ReadError {
            file: filename.clone(),
            error: e,
        })?;
        let string_contents =
            std::str::from_utf8(&contents).map_err(|e| ParserError::Utf8Error {
                file: filename.clone(),
                error: e,
            })?;

        let lexer = Lexer::from(string_contents);
        let mut parser = Parser::new(&file, lexer);
        let module = parser.parse_module()?;
        self.modules.insert(file.as_ref().to_path_buf(), module);

        Ok(())
    }
}
