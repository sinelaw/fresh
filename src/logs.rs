#[cfg(test)]
macro_rules! log {
    ($($arg:tt)*) => {
        println!("[{}:{}] {}", file!(), line!(), format_args!($($arg)*));
    };
}

#[cfg(not(test))]
pub struct LogState {
    pub file: Option<::std::fs::File>,
}

#[cfg(not(test))]
pub static LOG_STATE: std::sync::Mutex<LogState> = std::sync::Mutex::new(LogState { file: None });

#[cfg(not(test))]
macro_rules! log {
    ($($arg:tt)*) => { {
        let mut log_state  = $crate::logs::LOG_STATE.lock().unwrap();
        if log_state.file.is_none() {
            let file = ::std::fs::OpenOptions::new()
            .create(true)
            .append(true)
            .open("/tmp/editor.log")
            .unwrap();
            log_state.file = Some(file);
        }
        let str = format!("[{}:{}] {}\n", file!(), line!(), format_args!($($arg)*));
        ::std::io::Write::write(&mut log_state.file.as_mut().unwrap(), &str.into_bytes()).unwrap();
    } };
}

pub(crate) use log;
