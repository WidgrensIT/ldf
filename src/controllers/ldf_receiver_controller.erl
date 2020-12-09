-module(ldf_receiver_controller).
-export([
         message/1
        ]).

message(#{req := #{method := <<"POST">>} = Req,
        json := Json}) ->
    CL = binary_to_integer(cowboy_req:header(<<"content-length">>, Req, <<"0">>)),
    #{<<"id">> := MessageId} = Json,
    ok = ldf_db:add_message(MessageId, encode(Json), CL),
    {status, 200};
message(#{req := #{method := <<"GET">>}}) ->
    {ok, List} = ldf_db:get_messages(),
    Xml = [etsi103707:json_to_xml(decode(Json), CL) || #{payload := Json, content_length := CL} <- List],
    {json, 200, #{}, Xml}.

encode(Item) ->
    json:encode(Item, [maps, binary]).

decode(Item) ->
    json:decode(Item, [maps]).