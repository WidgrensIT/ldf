-module(etsi103707).

-export([json_to_xml/2]).

-include_lib("xmerl/include/xmerl.hrl").
%% =DEBUG REPORT==== 23-Nov-2020::13:45:33.254058 ===
%% incoming message: #{<<"chatId">> => <<"4f96ded6-6b8f-4f50-9589-9bcb83d86df0">>,
%%                     <<"id">> => <<"1e87fd7c-de4f-4e97-be27-2bdcdbad23b7">>,
%%                     <<"payload">> => <<"hi hi">>,
%%                     <<"sender">> => <<"9eccd7c2-85e0-4233-b31a-68f546923d9c">>,
%%                     <<"timestamp">> => 1606135533171}
json_to_xml(#{<<"chatId">> := ChatId,
              <<"id">> := MessageId,
              <<"payload">> := Payload,
              <<"sender">> := Sender,
              <<"timestamp">> := Timestamp},
              ContentLength) ->
    logger:debug("converting to 103 707"),
    Content = [
               #xmlElement{name = header},
               #xmlElement{name = payload,
                           attributes = [
                                         #xmlAttribute{
                                                       name = 'xsi:type',
                                                       value = [<<"MessagingPayload">>]
                                                      }
                                        ],
                            content = [core_parameters(Sender, <<"true">>, ChatId, integer_to_binary(Timestamp), MessageId, ContentLength)
                                      ]
                            }
                ],
    list_to_binary(lists:flatten(xmerl:export_simple([header(Content)], xmerl_xml))).



header(Content) ->
    #xmlElement{
                name = handoverItem,
                attributes = [
                              #xmlAttribute{
                                            name = xmlns,
                                            value = [<<"http://uri.etsi.org/03707/2020/02">>]
                                           },
                              #xmlAttribute{
                                            name = 'xmlns:xsi',
                                            value = [<<"http://www.w3.org/2001/XMLSchema-instance">>]
                                           }
                             ],
                content = Content
                }.

core_parameters(Sender, IsTargetedParty, Receiver, Timestamp, MessageId, ContentLength) ->
    #xmlElement{name = coreParameters,
                content = [message_sender(Sender, IsTargetedParty),
                           message_receiver(Receiver),
                           timestamp(Timestamp),
                           associated_binary_data(MessageId, ContentLength)]
               }.

message_sender(Sender, IsTargetedParty) ->
    #xmlElement{name = messageSender,
                content = [identifiers(Sender),
                           #xmlElement{name = isTargetedParty,
                                       content = [#xmlText{value = [IsTargetedParty]}]}]
               }.

identifiers(Identifier) ->
    #xmlElement{name = identifiers,
                content = [#xmlElement{name = identifier,
                                       content = [#xmlText{value = [Identifier]}]
                                      }
                          ]
               }.
message_receiver(Receiver) ->
    #xmlElement{name = messageReceiver,
                content = [identifiers(Receiver)]
               }.

timestamp(Timestamp) ->
    #xmlElement{name = timestamp,
                content = [#xmlText{value = [Timestamp]}]}.

associated_binary_data(MessageId, ContentLength) ->
    #xmlElement{name = associatedBinaryData,
                content = [#xmlElement{name = binaryObject,
                                       content = [#xmlElement{name = url,
                                                              content = [#xmlText{value = [<<"http://localhost:8095/message/", MessageId/binary>>]}]},
                                                  #xmlElement{name = cspDefinedIdentifier,
                                                              content = [#xmlText{value = [MessageId]}]},
                                                  #xmlElement{name = contentLength,
                                                              content = [#xmlText{value = [ContentLength]}]},
                                                  #xmlElement{name = contentType,
                                                              content = [#xmlText{value = [<<"plain/text">>]}]}]}]}.