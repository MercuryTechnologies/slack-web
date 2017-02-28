# Haskell bindings for the Slack web API

## Example

```
> import qualified Web.Slack as Slack
```

```
> import qualified Web.Slack.Api as Api
```

```
> manager <- Slack.mkManager
```

```
> :set -XRecordWildCards
> Slack.Cli{..} = Slack.mkCli
apiTest :: ...
authTest :: ...
channelsCreate :: ...
chatPostMessage :: ...
```

```
> Slack.run manager (apiTest Api.mkTestReq)
Right ...
```

```
> :set -XOverloadedStrings
> Slack.run manager (apiTest Api.mkTestReq { testReqFoo = Just "bar" })
Right ...
```
