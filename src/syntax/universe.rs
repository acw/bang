use crate::syntax::ast::*;
use crate::syntax::error::ParserError;
use crate::syntax::parse::Parser;
use crate::syntax::tokens::Lexer;
use memmap2::Mmap;
use std::collections::HashMap;
use std::path::{Path, PathBuf};

pub struct Universe {
    pub files: HashMap<PathBuf, Mmap>,
    pub modules: HashMap<PathBuf, Module>,
}

impl Default for Universe {
    fn default() -> Self {
        Universe {
            files: HashMap::new(),
            modules: HashMap::new(),
        }
    }
}

impl Universe {
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
