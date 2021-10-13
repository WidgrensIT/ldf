-module(ldf_receiver_controller).
-export([
         message/1
        ]).

message(#{req := #{method := <<"POST">>},
        json := #{<<"id">> := MessageId} = Json}) ->
    Message = case Json of
                  #{<<"payload">> := #{<<"url">> := Url} = Payload} ->
                        {ok, ChatliPath} = application:get_env(ldf, chatli_path),
                        Payload2 = maps:update(<<"url">>, <<ChatliPath/binary, Url/binary>>, Payload),
                        maps:update(<<"payload">>, Payload2, Json);
                    _ -> Json
              end,
    ok = ldf_db:add_message(encode(Message), MessageId),
    {status, 200};
message(#{req := #{method := <<"GET">>}}) ->
    {ok, List} = ldf_db:get_messages(),
    Xml = [etsi103707:json_to_xml(decode(Json)) || #{payload := Json} <- List],
    {json, 200, #{}, Xml}.

encode(Item) ->
    json:encode(Item, [maps, binary]).

decode(Item) ->
    json:decode(Item, [maps]).