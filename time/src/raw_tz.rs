use std::os::raw::{c_char, c_long, c_int};
use std::ffi::CStr;

#[repr(C)]
struct tm {
    tm_sec: c_int,          // seconds
    tm_min: c_int,          // minutes
    tm_hour: c_int,         // hours
    tm_mday: c_int,         // day of the month
    tm_mon: c_int,          // month
    tm_year: c_int,         // year
    tm_wday: c_int,         // day of the week
    tm_yday: c_int,         // day in the year
    tm_isdst: c_int,        // daylight saving time
    tm_gmtoff: c_long,      // offset from UTC
    tm_zone: *const c_char, // time zone abbreviation
}

extern "C" {
    fn time(t: *mut c_long) -> c_long;
    fn localtime_r(timep: *const c_long, result: *mut tm) -> *mut tm;
}

pub fn get_tz_name() -> String {
    unsafe {
        let current_time: c_long = 0;
        let mut local_time: tm = std::mem::zeroed();
        localtime_r(&current_time, &mut local_time);
        CStr::from_ptr(local_time.tm_zone).to_string_lossy().into_owned()
    }
}

pub fn get_tz_offset() -> i64 {
    unsafe {
        let current_time: c_long = 0;
        let mut local_time: tm = std::mem::zeroed();
        localtime_r(&current_time, &mut local_time);
        let gmtoff_seconds = local_time.tm_gmtoff;
        gmtoff_seconds
    }
}
