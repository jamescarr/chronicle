//// Logging utilities with process ID

import gleam/erlang/process.{type Pid}
import logging

/// Log an info message with the current process ID
pub fn info(message: String) -> Nil {
  let pid = process.self() |> pid_to_string
  logging.log(logging.Info, "[" <> pid <> "] " <> message)
}

/// Log a debug message with the current process ID
pub fn debug(message: String) -> Nil {
  let pid = process.self() |> pid_to_string
  logging.log(logging.Debug, "[" <> pid <> "] " <> message)
}

/// Log a warning message with the current process ID
pub fn warn(message: String) -> Nil {
  let pid = process.self() |> pid_to_string
  logging.log(logging.Warning, "[" <> pid <> "] " <> message)
}

/// Log an error message with the current process ID
pub fn error(message: String) -> Nil {
  let pid = process.self() |> pid_to_string
  logging.log(logging.Error, "[" <> pid <> "] " <> message)
}

/// Convert a Pid to a string representation
fn pid_to_string(pid: Pid) -> String {
  pid |> pid_to_list |> characters_to_binary
}

@external(erlang, "erlang", "pid_to_list")
fn pid_to_list(pid: Pid) -> List(Int)

@external(erlang, "unicode", "characters_to_binary")
fn characters_to_binary(chars: List(Int)) -> String
