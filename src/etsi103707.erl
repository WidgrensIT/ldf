-module(etsi103707).

-export([json_to_xml/1]).

-define(SCHEMAIDENTIFIER, <<"http://localhost:8090/schema/1.0/">>).

-include_lib("xmerl/include/xmerl.hrl").
%% =DEBUG REPORT==== 23-Nov-2020::13:45:33.254058 ===
%% incoming message: #{<<"chatId">> => <<"4f96ded6-6b8f-4f50-9589-9bcb83d86df0">>,
%%                     <<"id">> => <<"1e87fd7c-de4f-4e97-be27-2bdcdbad23b7">>,
%%                     <<"payload">> => <<"hi hi">>,
%%                     <<"sender">> => <<"9eccd7c2-85e0-4233-b31a-68f546923d9c">>,
%%                     <<"timestamp">> => 1606135533171}
json_to_xml(
    #{
        <<"chat_id">> := ChatId,
        <<"id">> := MessageId,
        <<"payload">> := Payload,
        <<"sender">> := Sender,
        <<"sender_info">> := SenderInfo,
        <<"timestamp">> := Timestamp
    } = Object
) ->
    logger:debug("converting to 103 707"),
    CspDefinedParameters = csp_defined_parameters(Object),
    logger:debug("payload: ~p", [Payload]),
    Mime =
        case Payload of
            #{<<"mime">> := [MimeType]} -> MimeType;
            _ -> <<"text/plain">>
        end,
    logger:debug("mime: ~p", [Mime]),
    ContentLength =
        case is_binary(Payload) of
            true ->
                integer_to_binary(byte_size(Payload));
            false ->
                case is_map(Payload) of
                    true ->
                        Json = thoas:encode(Payload),
                        integer_to_binary(byte_size(Json));
                    false ->
                        <<"0">>
                end
        end,
    Content = [
        #xmlElement{
            name = header,
            content = [
                #xmlElement{
                    name = 'applicationCorrelation',
                    content = [
                        #xmlElement{
                            name = 'applicationLevelID',
                            content = [#xmlText{value = <<"something">>}]
                        },
                        #xmlElement{
                            name = 'applicationSequenceNumber',
                            content = [#xmlText{value = <<"3">>}]
                        }
                    ]
                }
            ]
        },
        #xmlElement{
            name = payload,
            attributes = [
                #xmlAttribute{
                    name = 'xsi:type',
                    value = [<<"MessagingPayload">>]
                }
            ],
            content = [
                core_parameters(
                    Sender,
                    dummy,
                    ChatId,
                    calendar:system_time_to_rfc3339(
                        Timestamp,
                        [{unit, millisecond}]
                    ),
                    MessageId,
                    ContentLength,
                    Mime
                ),
                CspDefinedParameters
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

core_parameters(Sender, _, Receiver, Timestamp, MessageId, ContentLength, Mime) ->
    {IsTargetedPartyBool, SenderInfo} =
        case ldf_db:get_li_user_id(Sender) of
            {ok, User} -> {<<"true">>, User};
            _ -> {<<"false">>, #{}}
        end,
    logger:debug("sender info: ~p", [SenderInfo]),
    #xmlElement{
        name = coreParameters,
        content = [
            message_sender({Sender, SenderInfo}, IsTargetedPartyBool),
            message_receiver(Receiver),
            timestamp(Timestamp),
            associated_binary_data(MessageId, ContentLength, Mime)
        ]
    }.

message_sender({Sender, SenderInfo}, IsTargetedParty) ->
    #xmlElement{
        name = messageSender,
        content = [
            identifiers(Sender, SenderInfo),
            #xmlElement{
                name = isTargetedParty,
                content = [#xmlText{value = [IsTargetedParty]}]
            }
        ]
    }.

identifiers(Identifier, #{
    <<"email">> := Email,
    <<"phone_number">> := PhoneNumber,
    <<"user_agent">> := UserAgent
}) ->
    #xmlElement{
        name = identifiers,
        content = [
            #xmlElement{
                name = identifier,
                content = [#xmlText{value = [Identifier]}]
            },
            #xmlElement{
                name = identifier,
                content = [#xmlText{value = [Email]}]
            },
            #xmlElement{
                name = identifier,
                content = [#xmlText{value = [PhoneNumber]}]
            },
            #xmlElement{
                name = 'etsi707:identifier',
                content = [#xmlText{value = [UserAgent]}]
            }
        ]
    };
identifiers(Identifier, _) ->
    identifiers(Identifier).

identifiers(Identifier) ->
    #xmlElement{
        name = identifiers,
        content = [
            #xmlElement{
                name = identifier,
                content = [#xmlText{value = [Identifier]}]
            }
        ]
    }.
message_receiver(Receiver) ->
    #xmlElement{
        name = messageReceiver,
        content = [identifiers(Receiver)]
    }.

timestamp(Timestamp) ->
    #xmlElement{
        name = timestamp,
        content = [#xmlText{value = [Timestamp]}]
    }.

associated_binary_data(MessageId, ContentLength, Mime) ->
    Url = #xmlElement{
        name = url,
        content = [#xmlText{value = [<<"http://localhost:8095/message/", MessageId/binary>>]}]
    },
    #xmlElement{
        name = associatedBinaryData,
        content = [
            #xmlElement{
                name = binaryObject,
                content = [
                    Url,
                    #xmlElement{
                        name = cspDefinedIdentifier,
                        content = [#xmlText{value = [MessageId]}]
                    },
                    #xmlElement{
                        name = contentLength,
                        content = [#xmlText{value = [ContentLength]}]
                    },
                    #xmlElement{
                        name = contentType,
                        content = [#xmlText{value = [Mime]}]
                    }
                ]
            }
        ]
    }.

csp_defined_parameters(Object) ->
    Keys = maps:keys(Object),
    #xmlElement{
        name = cspDefinedParameters,
        content = [csp_defined_metadata(Keys)]
    }.

csp_defined_metadata(Keys) ->
    #xmlElement{
        name = cspDefinedMetadata,
        content = [
            schema_details(Keys),
            #xmlElement{
                name = xmlData,
                content = [chatli_defined_parameters(Keys)]
            }
        ]
    }.

schema_details(_Keys) ->
    #xmlElement{
        name = schemaDetails,
        content = [
            #xmlElement{
                name = schemaIdentifier,
                content = [#xmlText{value = [?SCHEMAIDENTIFIER]}]
            }
        ]
    }.

chatli_defined_parameters(Keys) ->
    #xmlElement{
        name = chatLiDefinedParameters,
        attributes = [
            #xmlAttribute{
                name = 'xmlns:xs',
                value = [?SCHEMAIDENTIFIER]
            }
        ],
        content = items(Keys, 1)
    }.

items(Keys, N) ->
    Nbin = integer_to_binary(N),
    case Keys of
        [] ->
            [];
        [Key | T] ->
            [
                #xmlElement{
                    name = binary_to_atom(<<"item", Nbin/binary>>, utf8),
                    content = [#xmlText{value = [Key]}]
                }
                | items(T, N + 1)
            ]
    end.
