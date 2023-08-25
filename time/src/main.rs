use std::time::{SystemTime, Duration};

struct Args {
    format: String,
    iso: bool,
    timezone: i64,
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
            format: parser.get_one::<String>("date_format_string").unwrap().to_string(),
            iso: parser.contains_id("iso"),
            timezone: *parser.get_one("timezone").unwrap(),
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
    let days = system.0;
    let num_days = system.1;
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

fn days_to_years(days_since_epoch: u64) -> u64 {
    let four_year_cycles = days_since_epoch / (365 * 4 + 1);
    let hundred_year_cycles = days_since_epoch / (365 * 100 + 24);
    let four_hundred_year_cycles = days_since_epoch / (365 * 400 + 97);
    let days = days_since_epoch - four_year_cycles + hundred_year_cycles - four_hundred_year_cycles;
    println!("days: {}, four_year_cycles: {}, hundred_year_cycles: {}, four_hundred_year_cycles: {}", days, four_year_cycles, hundred_year_cycles, four_hundred_year_cycles);
    days / 365
}

fn year(duration_since_unix_epoch: Duration, unix_epoch_repr: u64) -> u64 {
    let days_since_unix_epoch = duration_since_unix_epoch.as_secs() / 86400;
    let naive_years_since_unix_epoch = days_since_unix_epoch / 365;
    unix_epoch_repr
}

fn gregorian_year(duration_since_unix_epoch: Duration) -> u64 {
    year(duration_since_unix_epoch, 1970)
}

fn french_year(duration_since_unix_epoch: Duration) -> u64 {
    year(duration_since_unix_epoch, 178)
}

// works for both decimal and christian calendar
fn is_leap_year(year: u64) -> bool {
    year % 4 == 0 && (year % 100 != 0 || year % 400 == 0)
}

fn raw_day_in_year(duration_since_epoch: Duration) -> u64 {
    let days_since_epoch = duration_since_epoch.as_secs() / 86400;
    days_since_epoch % 365
}

fn decimal_year_date(duration_since_epoch: Duration) -> u64 {
    let date_in_year = raw_day_in_year(duration_since_epoch);
    let year = french_year(duration_since_epoch);
    if is_leap_year(year) && date_in_year > 59 {
        date_in_year - 1
    } else {
        date_in_year
    }
}

fn gregorian_year_date(duration_since_epoch: Duration) -> u64 {
    let date_in_year = raw_day_in_year(duration_since_epoch);
    let year = gregorian_year(duration_since_epoch);
    if is_leap_year(year - 1) {
        date_in_year - 1
    } else {
        date_in_year
    }
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

fn find_month<'a>(date: u64, system_months: &'a [(&'a str, u64, &'a str)]) -> &'a (&'a str, u64, &'a str) {
    for month in system_months.iter() {
        let month_offset = &month.1;
        if date >= *month_offset {
            return month
        }
    }
    panic!("Invalid date")
}

fn date_in_month<'a>(date: u64, system_months: &'a [(&'a str, u64, &'a str)]) -> u64 {
    let month = find_month(date, system_months);
    date - month.1
}

fn gregorian_date_in_month(duration_since_epoch: Duration) -> u64 {
    let day_in_year = gregorian_year_date(duration_since_epoch);
    date_in_month(day_in_year, GREG_MONTHS)
}

fn decimal_date_in_month(duration_since_epoch: Duration) -> u64 {
    let day_in_year = raw_day_in_year(duration_since_epoch);
    date_in_month(day_in_year, DECIMAL_MONTHS)
}

fn month<'a>(date: u64, system_months: &'a [(&'a str, u64, &'a str)], use_long: bool) -> &'a str {
    let month = find_month(date, system_months);
    if use_long {
        month.2
    } else {
        month.0
    }
}

fn gregorian_short_month<'a>(duration_since_epoch: Duration) -> &'a str {
    let day_in_year = gregorian_year_date(duration_since_epoch);
    month(day_in_year, GREG_MONTHS, false)
}

fn gregorian_long_month<'a>(duration_since_epoch: Duration) -> &'a str {
    let day_in_year = gregorian_year_date(duration_since_epoch);
    month(day_in_year, GREG_MONTHS, true)
}

fn decimal_short_month<'a>(duration_since_epoch: Duration) -> &'a str {
    let day_in_year = raw_day_in_year(duration_since_epoch);
    month(day_in_year, DECIMAL_MONTHS, false)
}

fn decimal_long_month<'a>(duration_since_epoch: Duration) -> &'a str {
    let day_in_year = raw_day_in_year(duration_since_epoch);
    month(day_in_year, DECIMAL_MONTHS, true)
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

fn decimal_hour(duration_since_epoch: Duration) -> u64 {
    (duration_since_epoch.as_secs() / 3600) % 10
}

fn decimal_minute(duration_since_epoch: Duration) -> u64 {
    (duration_since_epoch.as_secs() / 3600) % 1_000
}

fn decimal_second(duration_since_epoch: Duration) -> u64 {
    (duration_since_epoch.as_secs() / 3600) % 1_000_000
}

#[derive(PartialEq)]
enum NextIs {
    Format,
    French,
    Literal
}

fn interpret_format_string(format_string: &str, datetime: SystemTime, utc_offset: i64) -> String {
    let raw_system_duration = datetime.duration_since(SystemTime::UNIX_EPOCH).expect("Time went backwards");
    let duration = if utc_offset < 0 {
        raw_system_duration - Duration::from_secs(utc_offset.abs() as u64)
    } else {
        raw_system_duration + Duration::from_secs(utc_offset as u64)
    };
    let mut outstring = String::new();
    let mut next_is = NextIs::Literal;
    for chr in format_string.chars() {
        match next_is {
            NextIs::Format => {
                next_is = NextIs::Literal;
                match &chr {
                    '%' => outstring.push('%'),
                    'f' => {
                        next_is = NextIs::French;
                    },
                    'a' => outstring.push_str(seven_day_weekday_from_duration(duration)),
                    'A' => outstring.push_str(seven_day_long_weekday_from_duration(duration)),
                    'b' => outstring.push_str(gregorian_short_month(duration)),
                    'B' => outstring.push_str(gregorian_long_month(duration)),
                    'Y' => outstring.push_str(&gregorian_year(duration).to_string()),
                    'y' => outstring.push_str(&gregorian_year(duration).to_string()[2..]),
                    'd' => outstring.push_str(&gregorian_date_in_month(duration).to_string()),
                    'H' => outstring.push_str(&duodecimal_hour(duration).to_string()),
                    'I' => outstring.push_str(&(duodecimal_hour(duration) % 12).to_string()),
                    'M' => outstring.push_str(&duodecimal_minute(duration).to_string()),
                    'S' => outstring.push_str(&duodecimal_second(duration).to_string()),
                    _ => panic!("Unknown format specifier: %{}", chr)
                }
            },
            NextIs::French => {
                match &chr {
                    'A' => outstring.push_str(decade_weekday_from_duration(duration)),
                    'a' => outstring.push_str(decade_number_from_duration(duration)),
                    'b' => outstring.push_str(decimal_short_month(duration)),
                    'B' => outstring.push_str(decimal_long_month(duration)),
                    'Y' => outstring.push_str(&french_year(duration).to_string()),
                    'y' => outstring.push_str(&french_year(duration).to_string()[2..]),
                    'd' => outstring.push_str(&decimal_date_in_month(duration).to_string()),
                    'H' => outstring.push_str(&decimal_hour(duration).to_string()),
                    'M' => outstring.push_str(&decimal_minute(duration).to_string()),
                    'S' => outstring.push_str(&decimal_second(duration).to_string()),
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
    let format = if !args.iso { args.format } else { "%Y-%m-%dT%H:%M:%S%z".to_string() };
    let sys_time = SystemTime::now();
    let out_string = interpret_format_string(&format, sys_time, args.timezone);
    println!("{}", out_string);
}

#[test]
fn test_days_since_epoch_to_years() {
    assert_eq!(days_since_epoch_to_years(50), 0);
    assert_eq!(days_since_epoch_to_years(365), 0);
    assert_eq!(days_since_epoch_to_years(366), 1);
    assert_eq!(days_since_epoch_to_years(370), 1);
    assert_eq!(days_since_epoch_to_years(365 * 5 + 1), 4);
    assert_eq!(days_since_epoch_to_years(365 * 5 + 2), 5);
    assert_eq!(days_since_epoch_to_years((365) * 4 + 1) * 25, 99);
    assert_eq!(days_since_epoch_to_years((365) * 4 + 1) * 25 + 1, 99);
    assert_eq!(days_since_epoch_to_years((365) * 4 + 1) * 25, 100);
    assert_eq!(days_since_epoch_to_years((365) * 100 + 96) * 4, 399);
    assert_eq!(days_since_epoch_to_years((365) * 100 + 96) * 4 + 1, 399);
    assert_eq!(days_since_epoch_to_years((365) * 100 + 96) * 4 + 2, 400);
}

#[test]
fn test_num_leap_years_between() {
    assert_eq!(num_leap_years_between(1999 * 365, 2001 * 365), 0);
    assert_eq!(num_leap_years_between(2000 * 365, 2000 * 365), 0);
    assert_eq!(num_leap_years_between(2000 * 365, 2001 * 365), 0);
    // assert_eq!(num_leap_years_between(2000 * 365, 2005 * 365), 1);
}
