# Haskell bindings for the Slack web API

[![][1]][0]

[![][3]][2]
[![][5]][4]

[0]: https://circleci.com/gh/jpvillaisaza/slack-web
[1]: https://circleci.com/gh/jpvillaisaza/slack-web.svg?style=svg
[2]: https://www.stackage.org/lts/package/slack-web
[3]: https://www.stackage.org/package/slack-web/badge/lts
[4]: https://www.stackage.org/nightly/package/slack-web
[5]: https://www.stackage.org/package/slack-web/badge/nightly

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

## License

Licensed under the MIT license. See [LICENSE.md](LICENSE.md).
