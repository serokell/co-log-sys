# co-log-sys

This is a logging library based on [`co-log-core`](https://github.com/kowainik/co-log/tree/master/co-log-core)
that implements a syslog backend.

NOTE: It's somewhat opinionated in that:
- it uses the `fmt` library for formatting
- uses `universum` as a `Prelude` alternative

## Example of usage

Simple example of usage, for more info read about [`co-log`](https://github.com/kowainik/co-log/tree/master/co-log)

```haskell
example :: IO ()
example = withLogMessageSyslog basicSyslogConfig $ \logMessageSyslog -> do
    let logMessageStdout = cmap fmtMessageFlat logTextStdout
        logAction = logMessageSyslog <> logMessageStdout
    unLogAction logAction $ Message Emergency "NOTICE ME HERE"
  where
    basicSyslogConfig = SyslogConfig
        { collector = AutoLocal
        , facility = User
        , appName = "example"
        }
```

## Contributions

If you'd like to contribute or file a Bug Report, please read our [Contributors Guide](docs/CONTRIBUTING.md).

## LICENSE

[MPL 2.0](LICENSE.md)
