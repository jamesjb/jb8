//
// error.rs --- Emulator error type.
//
// Copyright (C) 2016, James Bielman <jamesjb@gmail.com>
// All Rights Reserved.
//
// Released under the "BSD3" license. See the file "LICENSE"
// for details.
//

//! Types for error handling within the emulator.
//!
//! This module implements Rust's `error::Error` interface using
//! a custom enumerated type `Error` to wrap all errors that can
//! escape from the public interface of the emulator.

use std::error;
use std::fmt;
use std::io;
use std::num;
use std::result;

/// A failure result from an emulator function.
#[derive(Debug)]
pub enum Error {
    IO(io::Error),
    Parse(num::ParseIntError),
}

/// Custom result type returned by emulator functions.
pub type Result<T> = result::Result<T, Error>;

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Error::IO(ref err) => write!(f, "IO error: {}", err),
            Error::Parse(ref err) => write!(f, "Invalid number: {}", err),
        }
    }
}

impl error::Error for Error {
    fn description(&self) -> &str {
        match *self {
            Error::IO(ref err) => err.description(),
            Error::Parse(ref err) => err.description(),
        }
    }

    fn cause(&self) -> Option<&error::Error> {
        match *self {
            Error::IO(ref err) => Some(err),
            Error::Parse(ref err) => Some(err),
        }
    }
}

impl From<io::Error> for Error {
    fn from(err: io::Error) -> Error {
        Error::IO(err)
    }
}

impl From<num::ParseIntError> for Error {
    fn from(err: num::ParseIntError) -> Error {
        Error::Parse(err)
    }
}
