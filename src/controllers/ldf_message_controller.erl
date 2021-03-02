-module(ldf_message_controller).
-export([
         message/1
        ]).

message(#{req := #{method := <<"GET">>,
                   bindings := #{messageid := MessageId}}}) ->
    {ok, MessageList} = ldf_db:get_message(MessageId),
    ObjectList = [ decode(Object) || #{ payload := Object } <- MessageList],
    {json, 200, #{}, ObjectList}.

decode(Item) ->
    json:decode(Item, [maps]).