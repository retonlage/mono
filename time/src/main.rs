use std::{time::{SystemTime, Duration}, print, println};

mod raw_tz;

struct Args {
    format: Option<String>,
    iso: bool,
    timezone: Option<i64>,
}

impl Args {
    fn parse() -> Self {
        let parser = clap::Command::new("myapp")
            .version("1.0")
            .author("tonlage")
            .about("print the system date and time")
            .arg(
                clap::Arg::new("iso")
                    .short('I')
                    .long("iso-8601")
                    .help("output date/time in ISO 8601 format."))
            .arg(
                clap::Arg::new("timezone")
                    .short('z')
                    .long("timezone")
                    .help("utc offset")
                    .allow_negative_numbers(true))
            .arg(
                clap::Arg::new("date_format_string")
                    .index(1)
                    .help("date format string"))
            .get_matches();
        Args {
            format: parser.get_one::<String>("date_format_string").map(|s| { s.to_string() }),
            iso: parser.contains_id("iso"),
            timezone: parser.get_one::<i64>("timezone").copied(),
        }
    }
}

const NUM_CHRISTIAN_WEEKDAYS: usize = 7;
const CHRISTIAN_WEEKDAYS: &'static [(&str, &str); NUM_CHRISTIAN_WEEKDAYS] = &[
        ("Thu", "Thursday"),
        ("Fri", "Friday"),
        ("Sat", "Saturday"),
        ("Sun", "Sunday"),
        ("Mon", "Monday"),
        ("Tue", "Tuesday"),
        ("Wed", "Wednesday"),
    ];
const CHRISTIAN_WEEK_SYSTEM: (&'static [(&str, &str)], usize) = (CHRISTIAN_WEEKDAYS, NUM_CHRISTIAN_WEEKDAYS);

const NUM_DECADE_WEEKDAYS: usize = 10;
const DECADE_WEEKDAYS: &'static [(&str, &str); NUM_DECADE_WEEKDAYS] = &[
        ("01", "Primidi"),
        ("02", "Duodi"),
        ("03", "Tridi"),
        ("04", "Quartidi"),
        ("05", "Quintidi"),
        ("06", "Sextidi"),
        ("07", "Septidi"),
        ("08", "Octidi"),
        ("09", "Nonidi"),
        ("10", "Decadi")];

const DECADE_WEEK_SYSTEM: (&'static [(&str, &str)], usize) = (DECADE_WEEKDAYS, NUM_DECADE_WEEKDAYS);

fn weekday<'a>(duration_since_epoch: Duration, system: (&'a[(&'a str, &'a str)], usize), use_long: bool) -> &str {
    let (days, num_days) = system;
    let days_since_epoch = duration_since_epoch.as_secs() / 86400;
    let weekday = days_since_epoch % num_days as u64;
    if use_long {
        &days[weekday as usize].1
    } else {
        &days[weekday as usize].0
    }
}

fn seven_day_weekday_from_duration(duration_since_epoch: Duration) -> &'static str {
    weekday(duration_since_epoch, CHRISTIAN_WEEK_SYSTEM, false)
}

fn seven_day_long_weekday_from_duration(duration_since_epoch: Duration) -> &'static str {
    weekday(duration_since_epoch, CHRISTIAN_WEEK_SYSTEM, true)
}

fn decade_number_from_duration(duration_since_epoch: Duration) -> &'static str {
    weekday(duration_since_epoch, DECADE_WEEK_SYSTEM, false)
}

fn decade_weekday_from_duration(duration_since_epoch: Duration) -> &'static str {
    weekday(duration_since_epoch, DECADE_WEEK_SYSTEM, true)
}

fn year_and_date(duration_since_epoch: Duration) -> (u64, u64) {
    let days_since_epoch = duration_since_epoch.as_secs() / 86400;
    let four_year_cycles = days_since_epoch / (365 * 4 + 1);
    let hundred_year_cycles = days_since_epoch / (365 * 100 + 24);
    let four_hundred_year_cycles = days_since_epoch / (365 * 400 + 97);
    let days = days_since_epoch - four_year_cycles + hundred_year_cycles - four_hundred_year_cycles;
    (days / 365, days % 365)
}

fn year(duration_since_unix_epoch: Duration, unix_epoch_repr: u64) -> u64 {
    year_and_date(duration_since_unix_epoch).0 + unix_epoch_repr
}

fn day_in_year(duration_since_epoch: Duration) -> u64 {
    year_and_date(duration_since_epoch).1
}

fn gregorian_year(duration_since_unix_epoch: Duration) -> u64 {
    year(duration_since_unix_epoch, 1970)
}

fn french_year(duration_since_unix_epoch: Duration) -> u64 {
    year(duration_since_unix_epoch, 178)
}

// works for both decimal and gregorian calendar
fn is_leap_year(year: u64) -> bool {
    year % 4 == 0 && (year % 100 != 0 || year % 400 == 0)
}

fn decimal_year_date(duration_since_epoch: Duration) -> u64 {
    let date_in_year = day_in_year(duration_since_epoch);
    let year = french_year(duration_since_epoch);
    if is_leap_year(year) {
        date_in_year - 1
    } else {
        date_in_year
    }
}

fn gregorian_year_date(duration_since_epoch: Duration) -> u64 {
    let date_in_year = day_in_year(duration_since_epoch);
    let year = gregorian_year(duration_since_epoch);
    if is_leap_year(year - 1) && date_in_year > 59 {
        date_in_year - 1
    } else {
        date_in_year
    }
}

fn gregorian_century(duration_since_epoch: Duration) -> u64 {
    (gregorian_year(duration_since_epoch) / 100) + 1
}

const NUM_GREG_MONTHS: usize = 12;
const GREG_MONTHS: &'static [(&str, u64, &str); NUM_GREG_MONTHS] = &[
    ("Jan",   0, "January"),
    ("Feb",  31, "February"),
    ("Mar",  59, "March"),
    ("Apr",  90, "April"),
    ("May", 120, "May"),
    ("Jun", 151, "June"),
    ("Jul", 181, "July"),
    ("Aug", 212, "August"),
    ("Sep", 243, "September"),
    ("Oct", 273, "October"),
    ("Nov", 304, "November"),
    ("Dec", 334, "December")
];

const NUM_DECIMAL_MONTHS: usize = 13;
const DECIMAL_MONTHS: &'static [(&str, u64, &str); NUM_DECIMAL_MONTHS] = &[
    ("Ven",   0, "Vendémiaire"),
    ("Bru",  30, "Brumaire"),
    ("Fri",  60, "Frimaire"),
    ("Niv",  90, "Nivôse"),
    ("Plu", 120, "Pluviôse"),
    ("Ven", 150, "Ventôse"),
    ("Ger", 180, "Germinal"),
    ("Flo", 210, "Floréal"),
    ("Pra", 240, "Prairial"),
    ("Mes", 270, "Messidor"),
    ("The", 300, "Thermidor"),
    ("Fru", 330, "Fructidor"),
    ("San", 360, "Sansculottides")
];

fn find_month(date: u64, system_months: &'static [(&'static str, u64, &'static str)]) -> (usize, &'static (&'static str, u64, &'static str)) {
    let mut last_month: &(&str, u64, &str) = &system_months[0];
    for (i, month) in system_months.iter().enumerate() {
        let month_offset = &month.1;
        if date <= *month_offset {
            return (i-1, last_month)
        }
        last_month = month;
    }
    (system_months.len() - 1, last_month)
}

fn date_in_month(date: u64, system_months: &'static [(&'static str, u64, &'static str)]) -> u64 {
    let (_, month) = find_month(date, system_months);
    date - month.1 + 1
}

fn gregorian_date_in_month(duration_since_epoch: Duration) -> u64 {
    let day_in_year = gregorian_year_date(duration_since_epoch);
    date_in_month(day_in_year, GREG_MONTHS)
}

fn decimal_date_in_month(duration_since_epoch: Duration) -> u64 {
    let day_in_year = day_in_year(duration_since_epoch);
    date_in_month(day_in_year, DECIMAL_MONTHS)
}

enum MonthDisplay { Number(usize), Long, Short }

fn month(date: u64, system_months: &'static [(&str, u64, &str)], display_type: MonthDisplay) -> String {
    let (i, month) = find_month(date, system_months);
    match display_type {
        MonthDisplay::Number(index) => (i + index).to_string(),
        MonthDisplay::Long => month.2.to_string(),
        MonthDisplay::Short => month.0.to_string(),
    }
}

fn gregorian_numeric_month_zero_index(duration_since_epoch: Duration) -> String {
    let day_in_year = day_in_year(duration_since_epoch);
    month(day_in_year, GREG_MONTHS, MonthDisplay::Number(0))
}

fn gregorian_numeric_month_one_index(duration_since_epoch: Duration) -> String {
    let day_in_year = day_in_year(duration_since_epoch);
    month(day_in_year, GREG_MONTHS, MonthDisplay::Number(1))
}

fn gregorian_short_month(duration_since_epoch: Duration) -> String {
    let day_in_year = gregorian_year_date(duration_since_epoch);
    month(day_in_year, GREG_MONTHS, MonthDisplay::Short)
}

fn gregorian_long_month(duration_since_epoch: Duration) -> String {
    let day_in_year = gregorian_year_date(duration_since_epoch);
    month(day_in_year, GREG_MONTHS, MonthDisplay::Long)
}

fn decimal_numeric_month_zero_index(duration_since_epoch: Duration) -> String {
    let day_in_year = day_in_year(duration_since_epoch);
    month(day_in_year, DECIMAL_MONTHS, MonthDisplay::Number(0))
}

fn decimal_numeric_month_one_index(duration_since_epoch: Duration) -> String {
    let day_in_year = day_in_year(duration_since_epoch);
    month(day_in_year, DECIMAL_MONTHS, MonthDisplay::Number(1))
}

fn decimal_short_month(duration_since_epoch: Duration) -> String {
    let day_in_year = day_in_year(duration_since_epoch);
    month(day_in_year, DECIMAL_MONTHS, MonthDisplay::Short)
}

fn decimal_long_month(duration_since_epoch: Duration) -> String {
    let day_in_year = day_in_year(duration_since_epoch);
    month(day_in_year, DECIMAL_MONTHS, MonthDisplay::Long)
}

// time

fn duodecimal_hour(duration_since_epoch: Duration) -> u64 {
    (duration_since_epoch.as_secs() / 3600) % 24
}

fn duodecimal_minute(duration_since_epoch: Duration) -> u64 {
    (duration_since_epoch.as_secs() / 60) % 60
}

fn duodecimal_second(duration_since_epoch: Duration) -> u64 {
    duration_since_epoch.as_secs() % 60
}

fn as_decimal_secs(duration: Duration) -> u64 {
    let millis = duration.as_millis();
    (millis / 864) as u64
}

fn decimal_hour(duration_since_epoch: Duration) -> u64 {
    let secs = as_decimal_secs(duration_since_epoch);
    (secs / 10_000) % 10
}

fn decimal_minute(duration_since_epoch: Duration) -> u64 {
    let secs = as_decimal_secs(duration_since_epoch);
    (secs / 100) % 100
}

fn decimal_second(duration_since_epoch: Duration) -> u64 {
    let secs = as_decimal_secs(duration_since_epoch);
    secs % 100
}

fn offset_hours(hours: i64,) -> String {
    format!("{:+03}", hours)
}

fn offset_hours_minutes(hours: i64, minutes: i64) -> String {
    format!("{:+03}:{:02}", hours, minutes)
}

fn offset_hours_minutes_no_colon(hours: i64, minutes: i64) -> String {
    format!("{:+03}{:02}", hours, minutes)
}

fn offset_hours_minutes_seconds(hours: i64, minutes: i64, secs: i64) -> String {
    format!("{:+03}:{:02}:{:02}", hours, minutes, secs)
}

fn offset_format(offset: i64, colons: u8) -> String {
    let secs = offset % 3600;
    let minutes = (offset % 3600) / 60;
    let hours = offset / 3600;
    match colons {
        0 => offset_hours_minutes_no_colon(hours, minutes),
        1 => offset_hours_minutes(hours, minutes),
        2 => offset_hours_minutes_seconds(hours, minutes, secs),
        3 => {
            if secs != 0 {
                offset_hours_minutes_seconds(hours, minutes, secs)
            } else if minutes != 0 {
                offset_hours_minutes(hours, minutes)
            } else {
                offset_hours(hours)
            }
        },
        _ => "%".to_owned() + &":".repeat(colons as usize) + "z",
    }

}

fn zeropad(unpadded: u64) -> String {
    format!("{:02}", unpadded)
}

fn interpret_format_string(format_string: &str, datetime: SystemTime, utc_offset: i64) -> String {
    let raw_system_duration = datetime.duration_since(SystemTime::UNIX_EPOCH).expect("Time went backwards.");
    let duration = if utc_offset < 0 {
        raw_system_duration - Duration::from_secs(utc_offset.abs() as u64)
    } else {
        raw_system_duration + Duration::from_secs(utc_offset as u64)
    };
    let mut outstring = String::new();
    #[derive(PartialEq)]
    enum NextIs {Format, French, Literal}
    #[derive(Default)]
    struct Flags {no_pad: bool, space_pad: bool, zero_pad: bool, plus_future: bool, prefer_upper: bool, prefer_invert_case: bool}
    let mut flags = Flags::default();
    let mut next_is = NextIs::Literal;
    let mut colons: u8 = 0;
    let format_string = if let Some(first) = format_string.chars().next() { // ignore leading +
        if first == '+' {
            &format_string[1..]
        } else { format_string }
    } else { format_string };
    for chr in format_string.chars() {
        match next_is {
            NextIs::Format => {
                next_is = NextIs::Literal;
                if chr != ':' && chr != 'z' {
                    colons = 0;
                }
                match &chr {
                    '%' => outstring.push('%'),
                    'f' => {
                        next_is = NextIs::French;
                    },
                    'a' => outstring.push_str(seven_day_weekday_from_duration(duration)),
                    'A' => outstring.push_str(seven_day_long_weekday_from_duration(duration)),
                    'b' => outstring.push_str(&gregorian_short_month(duration)),
                    'B' => outstring.push_str(&gregorian_long_month(duration)),
                    'C' => outstring.push_str(&gregorian_century(duration).to_string()),
                    'd' => outstring.push_str(&gregorian_date_in_month(duration).to_string()),
                    'D' => outstring.push_str(&interpret_format_string("%m/%d/%y", datetime, utc_offset)),
                    'Y' => outstring.push_str(&gregorian_year(duration).to_string()),
                    'y' => outstring.push_str(&gregorian_year(duration).to_string()[2..]),
                    'm' => outstring.push_str(&gregorian_numeric_month_one_index(duration).to_string()),
                    'H' => outstring.push_str(&zeropad(duodecimal_hour(duration))),
                    'I' => outstring.push_str(&zeropad(duodecimal_hour(duration) % 12)),
                    'M' => outstring.push_str(&zeropad(duodecimal_minute(duration))),
                    'S' => outstring.push_str(&zeropad(duodecimal_second(duration))),
                    ':' => {colons += 1; next_is = NextIs::Format},
                    'z' => outstring.push_str(&offset_format(utc_offset, colons)),
                    'Z' => outstring.push_str(&raw_tz::get_tz_name()),
                    _ => panic!("Unknown format specifier: %{}", chr)
                }
            },
            NextIs::French => {
                next_is = NextIs::Literal;
                match &chr {
                    'A' => outstring.push_str(decade_weekday_from_duration(duration)),
                    'a' => outstring.push_str(decade_number_from_duration(duration)),
                    'b' => outstring.push_str(&decimal_short_month(duration)),
                    'B' => outstring.push_str(&decimal_long_month(duration)),
                    'Y' => outstring.push_str(&french_year(duration).to_string()),
                    'y' => outstring.push_str(&french_year(duration).to_string()[2..]),
                    'm' => outstring.push_str(&decimal_numeric_month_one_index(duration).to_string()),
                    'd' => outstring.push_str(&decimal_date_in_month(duration).to_string()),
                    'H' => outstring.push_str(&decimal_hour(duration).to_string()),
                    'M' => outstring.push_str(&zeropad(decimal_minute(duration))),
                    'S' => outstring.push_str(&zeropad(decimal_second(duration))),
                    _ => panic!("Unknown format specifier: %f{}", chr)
                }
            },
            NextIs::Literal => {
                if chr == '%' {
                    next_is = NextIs::Format;
                } else {
                    outstring.push(chr);
                }
            }
        }
    }
    outstring
}

fn main() {
    let args = Args::parse();
    let iso_format = "%Y-%m-%dT%H:%M:%S%z".to_string();
    let format = if args.iso { iso_format } else { args.format.unwrap_or(iso_format) };
    let sys_time = SystemTime::now();
    let tz = if let Some(args_tz) = args.timezone {
        args_tz
    } else {
        raw_tz::get_tz_offset()
    };
    let out_string = interpret_format_string(&format, sys_time, tz);
    println!("{}", out_string);
}
