-module(ldf_message_controller).
-export([
         message/1
        ]).

message(#{req := #{method := <<"GET">>,
                   bindings := #{messageid := MessageId}}}) ->
    {ok, #{payload := Object}} = ldf_db:get_message(MessageId),
    {json, 200, #{}, decode(Object)}.

decode(Item) ->
    json:decode(Item, [maps]).