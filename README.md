# co-log-sys

This is a logging library based on [`co-log-core`](https://github.com/kowainik/co-log/tree/master/co-log-core)
that implements a syslog backend.

For more info about how [`co-log`](https://github.com/kowainik/co-log/tree/master/co-log)
works we advise you to read [its presentation blogpost](https://kowainik.github.io/posts/2018-09-25-co-log).

NOTE: It's somewhat opinionated in that:
- it uses the `fmt` library for formatting
- uses `universum` as a `Prelude` alternative

## Example of usage [↑](#co-log-sys)

This is a simple example using the `withLogMessageSyslog` function:

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

## Contributions [↑](#co-log-sys)

If you'd like to contribute or file a Bug Report, please read our [Contributors Guide](docs/CONTRIBUTING.md).

## License [↑](#co-log-sys)

[MPL 2.0](LICENSE.md)

## About Serokell [↑](#co-log-sys)

`co-log-sys` is maintained by [Serokell](https://serokell.io/).
We love open source software.
See which [services](https://serokell.io/#services) we provide and [drop us a line](mailto:hi@serokell.io) if you are interested.
