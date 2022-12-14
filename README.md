# Haskell bindings for the Slack web API

- Hackage: <https://hackage.haskell.org/package/slack-web>

- Slack web API: <https://api.slack.com/web>

## Example

```
> import qualified Web.Slack as Slack
```

```
> import qualified Web.Slack.Api as Api
```

```
> :set -XOverloadedStrings
```

```
> slackConfig <- Slack.mkSlackConfig token
```

```
> Slack.apiTest (Slack.slackConfigManager slackConfig) Api.mkTestReq
Right ...
```

```
> Slack.apiTest (Slack.slackConfigManager slackConfig) Api.mkTestReq { Api.testReqFoo = Just "bar" }
Right ...
```

## Contributing

This repository provides a `flake.nix` file which offers pre-commit hooks and
haskell-language-server. We use `fourmolu` for formatting, which is verified in
CI.

To use the flake, run `nix develop`, which will get you `cabal` and everything
else you need to work on `slack-web`.

Pull requests are welcome!

## License

Licensed under the MIT license. See [LICENSE.md](LICENSE.md).


