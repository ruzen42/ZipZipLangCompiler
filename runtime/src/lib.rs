#![no_std]

extern crate libc;

use libc::{c_char, write, exit};

pub unsafe fn text_length(s: *const c_char) -> usize {
    unsafe {
        let mut len = 0;
        while *s.add(len) != 0 {
            len += 1;
        }
        len
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn lang_print(s: *const c_char) {
    unsafe {
        let len = text_length(s);
        write(1, s as *const _, len);
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn lang_print_err(s: *const c_char) {
    unsafe {
        let len = text_length(s);
        write(2, s as *const _, len);
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn lang_exit(code: i32) -> ! {
    unsafe {
        exit(code);
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn lang_panic(msg: *const c_char) -> ! {
    unsafe {
        let prefix = b"panic: \0" as *const u8 as *const c_char; 

        lang_print_err(prefix);
        lang_print_err(msg);

        lang_exit(1);
    }
}

#[panic_handler]
unsafe fn panic(_: &core::panic::PanicInfo) -> ! {
    unsafe {

        exit(1); 
    }
}


