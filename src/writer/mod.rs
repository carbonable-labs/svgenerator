use std::{fs::File, io::Write};

use thiserror::Error;

pub mod cairo;

#[derive(Debug, Error)]
pub enum WriterError {
    #[error("failed to create file")]
    FailedToCreateFile,
    #[error("failed to write content to file")]
    FailedToWriteContent,
}

pub trait Writer {
    fn write(&self, name: &str, content: &str) -> Result<(), WriterError>;
}

pub struct FileWriter {}

impl Writer for FileWriter {
    fn write(&self, name: &str, content: &str) -> Result<(), WriterError> {
        let mut fs_file = File::create(name).map_err(|_| WriterError::FailedToCreateFile)?;
        fs_file
            .write_all(content.as_bytes())
            .map_err(|_| WriterError::FailedToWriteContent)?;
        Ok(())
    }
}

pub struct ConsoleWriter {}

impl Writer for ConsoleWriter {
    fn write(&self, name: &str, content: &str) -> Result<(), WriterError> {
        println!("{}", name);
        println!("{}", content);
        Ok(())
    }
}
