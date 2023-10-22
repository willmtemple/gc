#[cfg(windows)]
mod windows;

#[cfg(not(windows))]
fn main() {
    panic!("Not supported on this platform.")
}
