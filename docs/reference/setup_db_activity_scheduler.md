# Register the 30-minute recording script in Windows Task Scheduler

Creates a Windows Task Scheduler task that runs `record_db_activity.R`
every 30 minutes, passing database credentials as environment variables.

## Usage

``` r
setup_db_activity_scheduler(
  log_dir,
  db_user,
  db_password,
  task_name = "CafriplotsR_db_monitor",
  rscript_exe = NULL
)
```

## Arguments

- log_dir:

  Directory where the CSV logs will be written.

- db_user:

  Database username (stored as a Task Scheduler env var).

- db_password:

  Database password (stored as a Task Scheduler env var).

- task_name:

  Name for the scheduled task. Defaults to `"CafriplotsR_db_monitor"`.

- rscript_exe:

  Path to `Rscript.exe`. Auto-detected by default.

## Value

Invisible `TRUE` on success.

## Details

The task is created via `schtasks.exe` (Windows only). Run once from an
R session with administrator privileges, or add the task manually using
the Windows Task Scheduler GUI with the printed command.

## Examples

``` r
if (FALSE) { # \dontrun{
setup_db_activity_scheduler(
  log_dir     = "C:/db_logs/cafri",
  db_user     = "your_username",
  db_password = "your_password"
)
} # }
```
