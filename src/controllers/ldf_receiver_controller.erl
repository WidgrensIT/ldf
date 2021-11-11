-module(ldf_receiver_controller).
-export([
         create_message/1,
         get_message/1
        ]).

create_message(#{json := #{<<"id">> := MessageId} = Json}) ->
    Message = case Json of
                  #{<<"payload">> := #{<<"url">> := Url} = Payload} ->
                        {ok, ChatliPath} = application:get_env(ldf, chatli_path),
                        Payload2 = maps:update(<<"url">>, <<ChatliPath/binary, "/", Url/binary>>, Payload),
                        maps:update(<<"payload">>, Payload2, Json);
                    _ -> Json
              end,
    logger:debug("message: ~p~n", [Message]),
    EncodedMessage = encode(Message),
    logger:debug("Encoded message: ~p~n", [EncodedMessage]),
    ok = ldf_db:add_message(EncodedMessage, MessageId),
    {status, 200}.

get_message(_) ->
    {ok, List} = ldf_db:get_messages(),
    Xml = [etsi103707:json_to_xml(decode(Json)) || #{payload := Json} <- List],
    {json, 200, #{}, Xml}.

encode(Item) ->
    json:encode(Item, [maps, binary]).

decode(Item) ->
    json:decode(Item, [maps]).