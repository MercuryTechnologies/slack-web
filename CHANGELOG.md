# 1.5.0.0

## Breaking changes

- `PostMsgRsp` uses a `Text` instead of a `String` for the ts.
- `PostMsgReq` uses `Maybe` for the `text` since it is optional if blocks or
  attachments are provided.

The following fields are now Maybe since they have been found to be absent in
some responses:

- `ChannelConversation.channelSharedTeamIds`
- `ChannelConversation.channelIsMember`
- `ChannelConversation.channelNumMembers`

### Breaking changes since development versions

- The `SlackBlockSection` constructor has a new `Maybe` field for accessories.

## New features

- `chat.update` bindings [#112](https://github.com/MercuryTechnologies/slack-web/pull/112)
- `users.conversations` bindings [#109](https://github.com/MercuryTechnologies/slack-web/pull/109)
- Generic pagination, making new requests easy to paginate
  [#105](https://github.com/MercuryTechnologies/slack-web/pull/105)
- Expose an Internal module, allowing for new endpoints to be defined without
  changing slack-web
  [#99](https://github.com/MercuryTechnologies/slack-web/pull/99)
- Three experimental features:
  - Schemas for the Events API
    [#107](https://github.com/MercuryTechnologies/slack-web/pull/107)
  - Request verification for webhooks
    [#106](https://github.com/MercuryTechnologies/slack-web/pull/106)
  - Support for [Blocks](https://api.slack.com/block-kit) including a builder,
    headers, and accessories
    [#100](https://github.com/MercuryTechnologies/slack-web/pull/100),
    [#115](https://github.com/MercuryTechnologies/slack-web/pull/115),
    [#116](https://github.com/MercuryTechnologies/slack-web/pull/116)

## Bug fixes

- Conversation parse errors will pick the variant first, *then* let it fail.
  Messages now identify the correct variant and missing fields.
  [#108](https://github.com/MercuryTechnologies/slack-web/pull/108)

## Project updates

[Mercury](https://mercury.com) now maintains this library.

CI has been rewritten, and a Nix flake is now provided.

We have gathered a fair corpus of sample data from Slack through the course of
writing [Slacklinker](https://github.com/MercuryTechnologies/Slacklinker),
which has been used to write a new snapshot test suite.

Thanks to Daniel Brice, Dennis Hennen, Claire Violet, Evan Relf, Jake Keuhlen,
Rebecca Turner, Matthew Mongeau, and Kevin Tang for their contributions to this
release!

# 0.3.0.1

- Use Authorization header (44c3cf1)

# 0.1.0

## Methods

### New

- `api.test`
- `auth.test`
- `channels.create`
- `chat.postMessage`
