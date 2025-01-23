#[cfg(test)]
macro_rules! log {
    ($($arg:tt)*) => {
        println!("[{}:{}] {}", file!(), line!(), format_args!($($arg)*));
    };
}

#[cfg(not(test))]
macro_rules! log {
    ($($arg:tt)*) => {
        // In non-debug mode, you can replace this with logging to memory or a file
        // For now, it does nothing
    };
}

pub(crate) use log;
