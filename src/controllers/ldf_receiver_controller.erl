-module(ldf_receiver_controller).
-export([
         message/1
        ]).

message(#{req := #{method := <<"POST">>},
        json := Json}) ->
    #{<<"id">> := MessageId} = Json,
    ok = ldf_db:add_message(MessageId, encode(Json)),
    {status, 200};
message(#{req := #{method := <<"GET">>}}) ->
    {ok, List} = ldf_db:get_messages(),
    Xml = [etsi103707:json_to_xml(decode(Json)) || #{payload := Json} <- List],
    {json, 200, #{}, Xml}.

encode(Item) ->
    json:encode(Item, [maps, binary]).

decode(Item) ->
    json:decode(Item, [maps]).