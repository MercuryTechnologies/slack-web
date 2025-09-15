# 2.2.1.0 (2025-09-15)
* [#151](https://github.com/MercuryTechnologies/slack-web/151)
  Implement `conversations.member` API method.

# 2.2.0.0 (2025-03-21)
* [#145](https://github.com/MercuryTechnologies/slack-web/pull/145)
  Implement `conversations.info` API method.

  Breaking change: imIsUserDeleted is now Maybe to reflect reality.
* [#146](https://github.com/MercuryTechnologies/slack-web/pull/146)
  Implement `conversations.join` API method.

# 2.1.0.0 (2025-03-06)
* [#138](https://github.com/MercuryTechnologies/slack-web/pull/138)
  Implement `views.publish` method and App Home tab events.
* [#140](https://github.com/MercuryTechnologies/slack-web/pull/140)
  Implement `reactions.add` method.

  Breaking change: various places in the API using emoji now use an Emoji
  newtype.
* [#141](https://github.com/MercuryTechnologies/slack-web/pull/141)
  Include `response_metadata` in errors.
  This is a breaking change since it changes the type of `ResponseSlackError` and friends to add that field.
* [#142](https://github.com/MercuryTechnologies/slack-web/pull/142)
  Make the block builder previously used in the test suite public as an
  experimental module `Web.Slack.Experimental.Blocks.Builder`.

  This makes the DSL for specifying messages built with blocks a bit nicer.
* [#143](https://github.com/MercuryTechnologies/slack-web/pull/143)
  Add the `bot_profile` field to messages.

# 2.0.1.0 (2025-01-09)
* [#136](https://github.com/MercuryTechnologies/slack-web/pull/136)
  Bumps the version ranges for dependencies to be compatible with LTS 22.43.

# 2.0.0.4 (2024-09-15)
* [#135](https://github.com/MercuryTechnologies/slack-web/pull/135)
  Improves attachement support by providing clients wih the raw JSON value
  in case of a parse failure.

# 2.0.0.3 (2024-08-30)
* [#133](https://github.com/MercuryTechnologies/slack-web/pull/133)
  Adds attachment support for message event subscriptions.

# 2.0.0.2 (2024-08-29)
* [#132](https://github.com/MercuryTechnologies/slack-web/pull/132)
  Add support for bot_message event subtype.

# 2.0.0.0 (2023-10-26)

* [#123](https://github.com/MercuryTechnologies/slack-web/pull/123)
  Add support for section fields.

# 1.6.2.0 (2023-10-24)

* [#127](https://github.com/MercuryTechnologies/slack-web/pull/127)
  Add helper for the divider block type.

# 1.6.1.0 (2022-12-16)

* [#124](https://github.com/MercuryTechnologies/slack-web/pull/124)
  Parse [Slack incoming emails](https://slack.com/help/articles/206819278-Send-emails-to-Slack)
  without throwing an error.

# 1.6.0.0 (2022-12-14)

## Breaking changes

* `usersList` now takes a `User.ListReq` object. You can construct one with
  `data-default`'s `def` function to get the previous behaviour.

## New features

* [#120](https://github.com/MercuryTechnologies/slack-web/pull/120)
  Add file share event parsing support.

  Increase bounds on pretty-simple to 4.1 to mitigate a bug added in 4.0.

* [#121](https://github.com/MercuryTechnologies/slack-web/pull/120)
  Add pagination of `users.list`

# 1.5.0.1

* [#119](https://github.com/MercuryTechnologies/slack-web/pull/119) Fix a typo
  in rich text decoding.

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
