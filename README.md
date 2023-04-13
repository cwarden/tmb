# tmb

A tool to identify backups to purge when you have Too Many Backups.

See [tmb-go](https://github.com/cwarden/tmb-go) for a Go reimplementation.

## Using

Suppose you're making backups into timestamped directories using `rsync` or
`btrfs subvolume snapshot`, for example.  Then you realize that you have Too
Many Backups, and you decide that you want to keep the last seven daily
backups, the last four weekly backups, and a year of monthly backups.

`tmb` will tell you which backups to delete.

```
$ ls /path/to/backups
2015-03-09T20:07:36
2015-03-10T19:47:15
2015-03-24T21:42:46
2015-03-25T19:59:47
2015-03-26T21:18:34
2015-03-27T19:56:44
2015-03-29T20:28:53
2015-03-30T19:56:08
2015-03-31T20:04:54
2015-04-01T21:06:08
2015-04-02T21:08:06
2015-04-03T20:32:08
2015-04-04T20:05:30
2015-04-05T19:57:48
2015-04-06T20:30:19
2015-04-07T20:31:21
2015-04-08T20:40:38
2015-04-09T20:15:56
2015-04-10T20:26:00
2015-04-12T20:30:52
2015-04-13T20:21:48
2015-04-14T19:56:38
2015-04-15T19:54:44
2015-04-16T19:55:09
2015-04-17T20:02:40
2015-04-18T20:01:08
2015-04-19T20:17:57
2015-04-20T20:18:24
2015-04-21T20:02:01
2015-04-22T20:00:01
2015-04-24T21:22:28
2015-04-25T21:23:23
2015-04-26T21:17:03
2015-04-27T21:12:18
2015-04-29T20:41:45
2015-04-30T20:38:35
2015-05-01T20:15:19
2015-05-02T20:32:46
2015-05-04T20:56:21
last
$ ls /path/to/backups | tmb | xargs rm -rf
```

## Building

```
$ runhaskell Setup.lhs configure --prefix=$HOME --user
$ runhaskell Setup.lhs build
$ runhaskell Setup.lhs install
```

## TODO

* Convert date strings into objects containing date and original date string
* Read keepSpecs as arguments so seven daily, four weekly, and twelve monthly backups aren't hard-coded
* Add tests
